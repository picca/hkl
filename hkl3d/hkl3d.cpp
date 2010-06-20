/* This file is part of the hkl3d library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2010      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 */

#include <yaml.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <libgen.h>
#include <g3d/g3d.h>
#include <g3d/quat.h>
#include <g3d/matrix.h>

#include "hkl3d.h"
#include "btBulletCollisionCommon.h"
#include "BulletCollision/Gimpact/btGImpactCollisionAlgorithm.h"
#include "BulletCollision/Gimpact/btGImpactShape.h"

#ifdef USE_PARALLEL_DISPATCHER
# include "BulletMultiThreaded/SpuGatheringCollisionDispatcher.h"
# include "BulletMultiThreaded/PlatformDefinitions.h"
# include "BulletMultiThreaded/PosixThreadSupport.h"
# include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"
#endif

//#define SERIALIZE_TO_DISK
#ifdef SERIALIZE_TO_DISK
#include "btBulletWorldImporter.h"
#endif

/***************/
/* static part */
/***************/

static float identity[] = {1, 0, 0, 0,
			   0, 1, 0, 0,
			   0, 0, 1 ,0,
			   0, 0, 0, 1};

/**
 * hkl3d_config_release:
 * @config: 
 * @btCollisionWorld: 
 *
 * release the memory of an Hkl3dConfig. It also detach all btObjects from the btCollisionWorld
 **/
static void hkl3d_config_release(Hkl3DConfig *config, btCollisionWorld *btWorld)
{
	int i;
	Hkl3DObject *object;

	object = &config->objects[0];
	for(i=0; i<config->objects.size(); ++i){
		btWorld->removeCollisionObject(object->btObject);
#ifdef SERIALIZE_TO_DISK
		delete object->meshes;
#endif
		delete object->btShape;
		delete object->btObject;
		delete object->color;
		object++;
	}
}

static void hkl3d_object_init(Hkl3DObject *self, G3DObject *object, btCollisionShape *shape,
			      btCollisionObject *btObject, btTriangleMesh *trimesh, int id)
{
	int i;
	GSList *faces;
	G3DMaterial* material;

	// extract the color from the first face
	faces = object->faces;
	material = ((G3DFace *)faces->data)->material;

	// fill the hkl3d object structure.
	self->id = id;
	self->name = object->name;
	self->btObject = btObject;
	self->g3dObject = object;
	self->btShape = shape;
	self->meshes = trimesh;
	self->color = new btVector3(material->r, material->g, material->b);
	self->hide = object->hide;
	self->added = false;

	/*
	 * if the object already contain a transformation set the Hkl3DObject
	 * transformation with this transformation. Otherwise set it with the
	 * identity
	 */
	if(object->transformation)
		for(i=0; i<16; i++)
			self->transformation[i] = object->transformation->matrix[i];
	else
		for(i=0; i<16; i++)
			self->transformation[i] = identity[i];

	self->is_colliding = false;
}

static btTriangleMesh *trimesh_from_g3dobject(G3DObject *object)
{
	btTriangleMesh *trimesh;
	float *vertex;
	GSList *faces;			
	
	trimesh = new btTriangleMesh();
	trimesh->preallocateVertices(object->vertex_count);
	faces = object->faces;
	vertex = object->vertex_data;
	while(faces){
		G3DFace *face;
		
		face = (G3DFace*)faces->data;	
		btVector3 vertex0(vertex[3*(face->vertex_indices[0])],
				  vertex[3*(face->vertex_indices[0])+1],
				  vertex[3*(face->vertex_indices[0])+2]);
		btVector3 vertex1(vertex[3*(face->vertex_indices[1])], 
				  vertex[3*(face->vertex_indices[1])+1], 
				  vertex[3*(face->vertex_indices[1])+2]);
		btVector3 vertex2(vertex[3*(face->vertex_indices[2])],
				  vertex[3*(face->vertex_indices[2])+1], 
				  vertex[3*(face->vertex_indices[2])+2]);		
		trimesh->addTriangle(vertex0, vertex1, vertex2, true);

		faces = g_slist_next(faces);
	}

	return trimesh;
}

static btCollisionShape* shape_from_trimesh(btTriangleMesh *trimesh, int movable)
{
	btCollisionShape* shape;
	
	/*
	 * create the bullet shape depending on the static status or not of the piece
	 * static : do not move
	 * movable : connected to a HklGeometry axis.
	 */
	if (movable >= 0){	
		shape = new btGImpactMeshShape(trimesh);
		shape->setMargin(btScalar(0));
		shape->setLocalScaling(btVector3(1,1,1));
		(static_cast<btGImpactMeshShape*>(shape))->updateBound();
		(static_cast<btGImpactMeshShape*>(shape))->postUpdate();
	}else{
		shape = new btBvhTriangleMeshShape (trimesh, true);
		shape->setMargin(btScalar(0));
		shape->setLocalScaling(btVector3(1,1,1));
	}

	return shape;
}

static btCollisionObject * btObject_from_shape(btCollisionShape* shape)
{
	btCollisionObject *btObject;

	/* create the Object and add the shape */
	btObject = new btCollisionObject();
	btObject->setCollisionShape(shape);
	btObject->activate(true);

	return btObject;
}

struct ContactSensorCallback : public btCollisionWorld::ContactResultCallback
{
	ContactSensorCallback(btCollisionObject & collisionObject, Hkl3DObject & object)
		: btCollisionWorld::ContactResultCallback(),
		  collisionObject(collisionObject),
		  object(object)
		{ }
 
	btCollisionObject & collisionObject;
	Hkl3DObject & object;

	virtual btScalar addSingleResult(btManifoldPoint & cp,
					 const btCollisionObject *colObj0, int partId0, int index0,
					 const btCollisionObject *colObj1, int partId1, int index1)
		{
			if(colObj0 == &collisionObject
			   || colObj1 == &collisionObject) 
				object.is_colliding = true;		
			return 0; 
		}
};

/**
 * Hkl3D::Hkl3D:
 * @filename: 
 * @geometry: 
 *
 * 
 *
 * Returns: 
 **/
Hkl3D::Hkl3D(const char *filename, HklGeometry *geometry)
{
	this->geometry = geometry;
	_len = geometry->len;

	_context = g3d_context_new();
	this->model= g3d_model_new();

	// initialize the bullet part
	_btCollisionConfiguration = new btDefaultCollisionConfiguration();

#ifdef USE_PARALLEL_DISPATCHER
	int maxNumOutstandingTasks = 2;
	PosixThreadSupport::ThreadConstructionInfo constructionInfo("collision",
								    processCollisionTask,
								    createCollisionLocalStoreMemory,
								    maxNumOutstandingTasks);
	_btThreadSupportInterface = new PosixThreadSupport(constructionInfo);
	_btDispatcher = new SpuGatheringCollisionDispatcher(_btThreadSupportInterface,
							    maxNumOutstandingTasks,
							    _btCollisionConfiguration);
#else
	_btDispatcher = new btCollisionDispatcher(_btCollisionConfiguration);
#endif
	btGImpactCollisionAlgorithm::registerAlgorithm(_btDispatcher);

	btVector3 worldAabbMin(-1000,-1000,-1000);
	btVector3 worldAabbMax( 1000, 1000, 1000);

	_btBroadphase = new btAxisSweep3(worldAabbMin, worldAabbMax);

	_btWorld = new btCollisionWorld(_btDispatcher,
					_btBroadphase,
					_btCollisionConfiguration);

	if (filename)
		this->load_config(filename);
}

Hkl3D::~Hkl3D(void)
{
	int i;
	int len;

	// _remove objects from the collision world and delete all objects and shapes
	len = this->configs.size();
	for(i=0; i<len; ++i)
		hkl3d_config_release(&this->configs[i], _btWorld);

	if (_btWorld) delete _btWorld;
	if (_btBroadphase) delete _btBroadphase;
	if (_btDispatcher) delete _btDispatcher;
#ifdef USE_PARALLEL_DISPATCHER
	if (_btThreadSupportInterface){
		//delete _btThreadSupportInterface;
		//_btThreadSupportInterface = 0;
	}
#endif
	if (_btCollisionConfiguration) delete _btCollisionConfiguration;
	g3d_model_free(this->model);
	g3d_context_free(_context); 
}

Hkl3DConfig *Hkl3D::add_model_from_file(const char *filename, const char *directory)
{	
	G3DModel * model;
	G3DObject *object;
	G3DMaterial *material;
	char current[PATH_MAX];

	/* first set the current directory using the directory parameter*/
	getcwd(current, PATH_MAX);
	chdir(directory);
	model = g3d_model_load_full(_context, filename, 0);
	chdir(current);

	/* concatenate the added Model with the one from Hkl3D */ 
	this->model->objects = g_slist_concat(this->model->objects, model->objects);
 	this->model->materials = g_slist_concat(this->model->materials, model->materials);

	/* update the Hkl3D internals from the model */
	this->init_internals(model, filename);
	
	/* if resp == true there is a problem in the diffractometer model. */
	bool resp = this->is_colliding();

	return &this->configs.back();
}

void Hkl3D::load_config(const char *filename)
{
	int j;
	int newFile=0;
	int endEvent = 0;
	yaml_parser_t parser;
	yaml_event_t input_event;
	FILE *file;
	char *dirc;
	char *dir;

	/* Clear the objects. */
	memset(&parser, 0, sizeof(parser));
	memset(&input_event, 0, sizeof(input_event));

	file = fopen(filename, "rb");
	if (!file){
		fprintf(stderr, "Could not open the %s config file\n", filename);
		return;
	}

	if (!yaml_parser_initialize(&parser))
		fprintf(stderr, "Could not initialize the parser object\n");
	yaml_parser_set_input_file(&parser, file);

	/* 
	 * compute the dirname of the config file as all model files
	 * will be relative to this directory
	 */
	dirc = strdup(filename);
	dir = dirname(dirc);

	while(!endEvent){	
		Hkl3DConfig *config;

		/* Get the next event. */
		yaml_parser_parse(&parser, &input_event);
		
		/* Check if this is the stream end. */
		if(input_event.type == YAML_STREAM_END_EVENT)
			endEvent = 1;
		if(input_event.type == YAML_SCALAR_EVENT){
		
			if(!strcmp((const char *)input_event.data.scalar.value, "FileName")){
				yaml_parser_parse(&parser, &input_event);				

				/* the add form file method create a default Hkl3DConfig and add it to the HKL3D */
				/* we just need to update this config with the values from the configuration file */
				config = this->add_model_from_file((const char *)input_event.data.scalar.value, dir);
				j=0;
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Id")){	
				yaml_parser_parse(&parser, &input_event);
				config->objects[j].id = atoi((const char *)input_event.data.scalar.value);
				j++;
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Name")){	
				yaml_parser_parse(&parser, &input_event);
				config->objects[j-1].name = (const char *)input_event.data.scalar.value;
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Transformation")){
				int k;

				yaml_parser_parse(&parser, &input_event);
				for(k=0; k<16; k++){
					yaml_parser_parse(&parser, &input_event);
					if(input_event.type == YAML_SCALAR_EVENT)
						config->objects[j-1].transformation[k] = atof((const char *)input_event.data.scalar.value);
				}	
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Hide")){
				yaml_parser_parse(&parser, &input_event);
				config->objects[j-1].hide = strcmp((const char *)input_event.data.scalar.value, "no");
			}	
		}	
	}
	free(dirc);
	yaml_event_delete(&input_event);
    	yaml_parser_delete(&parser);
	fclose(file);
}

void Hkl3D::save_config(const char *filename)
{
	int i;

	for(i=0; i<this->configs.size(); i++){
		int j;
		char number[64];
		int properties1, key1, value1,seq0;
		int root;
		time_t now;
		yaml_emitter_t emitter;
		yaml_document_t output_document;
		yaml_event_t output_event;
		FILE * file;

		memset(&emitter, 0, sizeof(emitter));
		memset(&output_document, 0, sizeof(output_document));
		memset(&output_event, 0, sizeof(output_event));
	
		if (!yaml_emitter_initialize(&emitter)) 
			fprintf(stderr, "Could not inialize the emitter object\n");
	 
		/* Set the emitter parameters */
		file = fopen(filename, "a+");
		yaml_emitter_set_output_file(&emitter, file);
		yaml_emitter_open(&emitter);
	
		/* Create an output_document object */
		if (!yaml_document_initialize(&output_document, NULL, NULL, NULL, 0, 0))
			fprintf(stderr, "Could not create a output_document object\n");
	
		/* Create the root of the config file */ 
		time(&now);
		root = yaml_document_add_sequence(&output_document,
						  (yaml_char_t *)ctime(&now),
						  YAML_BLOCK_SEQUENCE_STYLE);

		/* create the property of the root sequence */
		properties1 = yaml_document_add_mapping(&output_document,
							(yaml_char_t *)YAML_MAP_TAG,
							YAML_BLOCK_MAPPING_STYLE);

		yaml_document_append_sequence_item(&output_document, root, properties1);
	
		/* add the map key1 : value1 to the property */
		key1 = yaml_document_add_scalar(&output_document,
						NULL,
						(yaml_char_t *)"FileName", 
						-1, 
						YAML_PLAIN_SCALAR_STYLE);
		value1 = yaml_document_add_scalar(&output_document,
						  NULL,
						  (yaml_char_t *)this->configs[i].filename,
						  -1, 
						  YAML_PLAIN_SCALAR_STYLE);
		yaml_document_append_mapping_pair(&output_document, properties1, key1, value1);

		/* add the map key1 : seq0 to the first property */
		key1 = yaml_document_add_scalar(&output_document,
						NULL,
						(yaml_char_t *)"Objects",
						-1,
						YAML_PLAIN_SCALAR_STYLE);
		/* create the sequence of objects */
		seq0 = yaml_document_add_sequence(&output_document,
						  (yaml_char_t *)YAML_SEQ_TAG,
						  YAML_BLOCK_SEQUENCE_STYLE);
		for(j=0; j<this->configs[i].objects.size(); j++){
			int k;
			int properties;
			int key;
			int value;
			int seq1;
		
			properties = yaml_document_add_mapping(&output_document,
							       (yaml_char_t *)YAML_MAP_TAG,
							       YAML_BLOCK_MAPPING_STYLE);
			yaml_document_append_sequence_item(&output_document,seq0, properties);

			key = yaml_document_add_scalar(&output_document, 
						       NULL,
						       (yaml_char_t *)"Id", -1, 
						       YAML_PLAIN_SCALAR_STYLE);
		
			sprintf(number, "%d", this->configs[i].objects[j].id);
			value = yaml_document_add_scalar(&output_document,
							 NULL,
							 (yaml_char_t *)number,
							 -1, 
							 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);

			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t *)"Name", 
						       -1, 
						       YAML_PLAIN_SCALAR_STYLE);
			value = yaml_document_add_scalar(&output_document,
							 NULL,
							 (yaml_char_t *)this->configs[i].objects[j].name,
							 -1, 
							 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);

			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t *)"Transformation",
						       -1,
						       YAML_PLAIN_SCALAR_STYLE);
			seq1 = yaml_document_add_sequence(&output_document, 
							  (yaml_char_t *)YAML_SEQ_TAG,
							  YAML_FLOW_SEQUENCE_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties, key, seq1);
			for(k=0; k<16; k++){
				sprintf(number, "%f", this->configs[i].objects[j].transformation[k]);
				value = yaml_document_add_scalar(&output_document,
								 NULL,
								 (yaml_char_t *)number, 
								 -1, 	
								 YAML_PLAIN_SCALAR_STYLE);
				yaml_document_append_sequence_item(&output_document,seq1,value);
			}
	
			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t *)"Hide",
						       -1,
						       YAML_PLAIN_SCALAR_STYLE);
			if(this->configs[i].objects[j].hide)
				value = yaml_document_add_scalar(&output_document,
								 NULL,
								 (yaml_char_t *)"yes",
								 -1,
								 YAML_PLAIN_SCALAR_STYLE);
			else
				value = yaml_document_add_scalar(&output_document,
								 NULL,
								 (yaml_char_t *)"no",
								 -1,
								 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);
		}
		yaml_document_append_mapping_pair(&output_document, properties1, key1, seq0);

		/* flush the document */
		yaml_emitter_dump(&emitter, &output_document);
		fclose(file);

		yaml_document_delete(&output_document);
		yaml_emitter_delete(&emitter);
	}
}

void Hkl3D::apply_transformations(void)
{
	int i;
	int k;
	struct timeval debut, fin, dt;

	// set the right transformation of each objects and get numbers
	gettimeofday(&debut, NULL);
	for(i=0; i<this->geometry->holders_len; i++){
		size_t j;
		btQuaternion btQ(0, 0, 0, 1);

		size_t len = this->geometry->holders[i].config->len;
		for(j=0; j<len; j++){
			size_t k;
			size_t idx = this->geometry->holders[i].config->idx[j];
			HklAxis *axis = &this->geometry->axes[idx];
			G3DMatrix G3DM[16];
			
			// convertion beetween hkl -> bullet coordinates
			btQ *= btQuaternion(-axis->q.data[1],
					    axis->q.data[3],
					    axis->q.data[2],
					    axis->q.data[0]);

			// move each object connected to that hkl Axis.
			for(k=0; k<_movingBtCollisionObjects[idx].size(); ++k){
				_movingBtCollisionObjects[idx][k]->getWorldTransform().setRotation(btQ);
				_movingBtCollisionObjects[idx][k]->getWorldTransform().getOpenGLMatrix( G3DM );
				memcpy(_movingG3DObjects[idx][k]->transformation->matrix, &G3DM[0], sizeof(G3DM));
			}
		}
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &dt);
	fprintf(stdout, "transformation (%f ms)", dt.tv_sec*1000.+dt.tv_usec/1000.);
}

bool Hkl3D::is_colliding(void)
{
	int i;
	int j;
	bool res = true;
	struct timeval debut, fin, dt;
	int numManifolds;

	//apply geometry transformation
	this->apply_transformations();
	// perform the collision detection and get numbers
	gettimeofday(&debut, NULL);
	if(_btWorld){
		_btWorld->performDiscreteCollisionDetection();
		_btWorld->updateAabbs();
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &dt);
	fprintf(stdout, " collision (%f ms)", dt.tv_sec*1000.+dt.tv_usec/1000.);
	
	numManifolds = _btWorld->getDispatcher()->getNumManifolds();

	/* reset all the collisions */
	for(i=0; i<this->configs.size(); i++)
		for(j=0; j<this->configs[i].objects.size(); j++)
			this->configs[i].objects[j].is_colliding = false;

	/* check all the collisions */
	for(i=0; i<this->configs.size(); i++)
		for(j=0; j<this->configs[i].objects.size(); j++){
			Hkl3DObject & object = this->configs[i].objects[j];
			ContactSensorCallback callback(*object.btObject, object);
			_btWorld->contactTest(object.btObject, callback);
		}		
	fprintf(stdout, " manifolds (%d)\n", numManifolds);

	return numManifolds == 0;
}

/*
 * Initialize the bullet collision environment.
 * create the Hkl3DObjects
 * create the Hkl3DConfigs
 */
void Hkl3D::init_internals(G3DModel *model, const char *filename)
{
	Hkl3DConfig config;
	GSList *objects; // lets iterate from the first object.

	// first initialize the _movingBtCollisionObjects and _movingG3DObjects with the right len.
	_movingBtCollisionObjects.resize(_len);
	_movingG3DObjects.resize(_len);

	objects = model->objects;
	while(objects){
		G3DObject *object;

		object = (G3DObject*)objects->data;
		if(object->vertex_count){			
			int id;
			int idx;			
			btCollisionShape *shape;
			btCollisionObject *btObject;
			btTriangleMesh *trimesh;
			Hkl3DObject hkl3dObject;

			trimesh = trimesh_from_g3dobject(object);
			idx = hkl_geometry_get_axis_idx_by_name(this->geometry, object->name);
			shape = shape_from_trimesh(trimesh, idx);
			btObject = btObject_from_shape(shape);
			
			/* fill movingCollisionObject and movingG3DObjects vectors for transformations */
			if(idx >= 0){
				_movingBtCollisionObjects[idx].push_back(btObject);
				object->transformation = g_new0(G3DTransformation, 1);
				_movingG3DObjects[idx].push_back(object);
			}
			id = g_slist_index(model->objects, object);
			hkl3d_object_init(&hkl3dObject, object, shape, btObject, trimesh, id);

			// insert collision Object in collision world
			_btWorld->addCollisionObject(hkl3dObject.btObject);
			hkl3dObject.added = true;
			
			// remembers objects to avoid memory leak
			config.objects.push_back(hkl3dObject);	 
		}
		objects = g_slist_next(objects);
	}
	
	config.filename = filename;
	this->configs.push_back(config);
}

#ifdef SERIALIZE_TO_DISK

static void serializeToDisk(btCollisionShape *shape, const char *name, const char *filename)
{
	int maxSerializeBufferSize = 1024*1024*5;
	btDefaultSerializer* serializer;
	FILE* file;

	serializer = new btDefaultSerializer(maxSerializeBufferSize);
	serializer->startSerialization();
	file = fopen(filename,"a+");

	serializer->registerNameForPointer(shape, name);
	shape->serializeSingleShape(serializer);

	serializer->finishSerialization();			
	fwrite(serializer->getBufferPointer(), serializer->getCurrentBufferSize(), 1, file);	
	fclose(file);
}

void Hkl3D::importFromBulletFile(const char *filename)
{
	int k;
	int idx;
	Hkl3DConfig hkl3dConfig;
	btCompoundShape* shape;
	btCollisionObject* btObject;
	btBulletWorldImporter import(0);

	// first initialize the _movingBtCollisionObjects and _movingG3DObjects with the right len.
	_movingBtCollisionObjects.resize(_len);
	_movingG3DObjects.resize(_len);
	if (import.loadFile(filename))
	{
		int numCollisionShapes = import.getNumCollisionShapes();
		std::cout<<numCollisionShapes<<std::endl;	
		for (k=0;k<numCollisionShapes;k++)
		{
			GSList *objects;
			objects = _model->objects;
			//Import collision shape
			shape =static_cast<btCompoundShape*>(import.getCollisionShapeByIndex(k));
			shape->setMargin(btScalar(0));
			shape->setLocalScaling(btVector3(1,1,1));
			//shape->updateBound();
			
			// create the Object and add the shape
			btObject = new btCollisionObject();
			btObject->setCollisionShape(shape);
			btObject->activate(true);
			// now populate also the moving part
			const char* shapeName = import.getNameForPointer(shape);
			bool found = false;
			G3DObject *object;
			GSList *faces;
			G3DMaterial *material;
			while(objects){	  
				object = (G3DObject *)objects->data;
				if(!strcmp(object->name, shapeName)){
					found = true;
					break;
				}
				objects = g_slist_next(objects);
			}
			idx = hkl_geometry_get_axis_idx_by_name(_geometry, shapeName);
			if (idx >= 0 && found){
				_movingBtCollisionObjects[idx].push_back(btObject);
				object->transformation = g_new0(G3DTransformation, 1);
				_movingG3DObjects[idx].push_back(object);
			}
			//extract object color
			faces = object->faces;
			material = ((G3DFace *)faces->data)->material;
			// create hkl3d object structure.
			HKL3DObject hkl3dObject;
			if(found){
				// create hkl3d object structure.
				HKL3DObject hkl3dObject;
				hkl3dObject.collisionObject = btObject;
				hkl3dObject.gObject = object;
				hkl3dObject.collisionShape = shape;
				hkl3dObject.color = new btVector3(material->r, material->g, material->b);
				hkl3dObject.name = object->name;
				hkl3dObject.hide = object->hide;
				hkl3dObject.AddedInWorldCollision = false;

				/*
				 * if the object already contain a transformation set the Hkl3DObject
				 * transformation with this transformation. Otherwise set it with the
				 * identity
				 */
				if(object->transformation)
					for(k=0; k<16; k++)
						hkl3dObject.transformation[k] = object->transformation->matrix[k];
				else
					for(k=0; k<16; k++)
						hkl3dObject.transformation[k] = identity[k];

				hkl3dObject.is_colliding = false;

				// insert collision Object in collision world
				_btCollisionWorld->addCollisionObject(hkl3dObject.collisionObject);
				hkl3dObject.AddedInWorldCollision = true;
			
				// remembers objects to avoid memory leak			
				hkl3dConfig.hkl3dObjects.push_back(hkl3dObject);
			}
			// remembers objects to avoid memory leak			
			hkl3dConfig.fileName=fileName;
			this->_hkl3dConfigs.push_back(hkl3dConfig);
	
		}
	}
	else
		std::cout<<"Error while loading bullet file"<<std::endl;
}
#endif
