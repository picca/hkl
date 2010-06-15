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

#define SERIALIZE_TO_DISK
#ifndef SERIALIZE_TO_DISK
#include "btBulletWorldImporter.h"
#endif

struct ContactSensorCallback : public btCollisionWorld::ContactResultCallback
{
	ContactSensorCallback(btCollisionObject & collisionObject,Hkl3D & hkl3d,int k)
		: btCollisionWorld::ContactResultCallback(), collisionObject(collisionObject),hkl3d(hkl3d),k(k) { } 
	btCollisionObject & collisionObject;
	Hkl3D & hkl3d;
	int k;
	virtual btScalar addSingleResult(btManifoldPoint& cp,
					 const btCollisionObject* colObj0,int partId0,int index0,
					 const btCollisionObject* colObj1,int partId1,int index1)
		{
			if(colObj0==&collisionObject||colObj1==&collisionObject) 
				hkl3d._hkl3dObjects[k].is_colliding=true;		
			return 0; 
		}
};

Hkl3D::Hkl3D(void)
{


}

Hkl3D::Hkl3D(const char *filename, HklGeometry *geometry)
{
	_geometry = geometry;
	_len = HKL_LIST_LEN(geometry->axes);
	_model= g3d_model_new();
	_model->materials=NULL;
	_model->objects=NULL;

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

	_btCollisionWorld = new btCollisionWorld(_btDispatcher,
						 _btBroadphase,
						 _btCollisionConfiguration);	
}

Hkl3D::~Hkl3D(void)
{
	size_t i;
	size_t len;

	len = _hkl3dObjects.size();

	// detach the objects from the collision world
	for(i=0; i<len; ++i)
		_btCollisionWorld->removeCollisionObject(_hkl3dObjects[i].collisionObject);

	// delete all objects and shapes
	for(i=0; i<len; ++i){
#ifdef SERIALIZE_TO_DISK
		delete _hkl3dObjects[i].meshes;
#endif
		delete _hkl3dObjects[i].collisionShape;
		delete _hkl3dObjects[i].collisionObject;
		delete _hkl3dObjects[i].color;
	}

	if (_btCollisionWorld) delete _btCollisionWorld;
	if (_btBroadphase) delete _btBroadphase;
	if (_btDispatcher) delete _btDispatcher;
#ifdef USE_PARALLEL_DISPATCHER
	if (_btThreadSupportInterface){
		//delete _btThreadSupportInterface;
		//_btThreadSupportInterface = 0;
	}
#endif
	if (_btCollisionConfiguration) delete _btCollisionConfiguration;
	g3d_model_free(_model);
}

void Hkl3D::addFromFile(const char *fileName)
{
	G3DContext * context;
	G3DModel * model;
	GSList *objects;
	GSList *materials;
	G3DObject *object;
	G3DMaterial *material;

	//Hkl3DConfig hkl3dConfig;
	
	//hkl3dConfig.fileName=fileName;
	context=g3d_context_new();
	// read the model from the file	
	model = g3d_model_load_full(context,fileName,G3D_MODEL_SCALE);
	objects = model->objects;
/*
	while(objects){

		G3DObject *object;
		object = (G3DObject*)objects->data;
		Object objectConfig;
		objectConfig.id=g_slist_index (model->objects,object);
		objectConfig.name=object->name;
		objectConfig.hide=object->hide;
		if(object->transformation){
			for(int k=0;k<16;k++)
				objectConfig.transformation[k]=object->transformation->matrix[k];
		}
		else{//set identity
			for(int k=0;k<16;k++){
				if(k==0||k==5||k==10||k==15)
					objectConfig.transformation[k]=1;
				else
					objectConfig.transformation[k]=0;	
			}		
		}
		hkl3dConfig.objects.push_back(objectConfig);
		objects = g_slist_next(objects);
	}
	this->_hkl3dConfigs.push_back(hkl3dConfig);
*/
	materials=model->materials;
	this->_model->objects=g_slist_concat (this->_model->objects,model->objects);
 	this->_model->materials=g_slist_concat(this->_model->materials,model->materials);
	this->parseYamlFile();
	this->emitObjectInYamlFile();
	this->loadModelInCollisionWorld(model);
	
	
	// if resp == true there is a problem in the diffractometer model.
	bool resp = this->is_colliding();
	
}

void Hkl3D::parseYamlFile(void)
{
	int i = 0;
	int newFile=0;
	int endEvent = 0;
	yaml_parser_t parser;
	yaml_event_t input_event;
	FILE *fileIn;
	Hkl3DConfig hkl3dConfig;
	Object objectConfig;

	/* Clear the objects. */

	memset(&parser, 0, sizeof(parser));
	memset(&input_event, 0, sizeof(input_event));

	fileIn = fopen("config.yaml", "rb");
	if (!yaml_parser_initialize(&parser))
		fprintf(stderr, "Could not initialize the parser object\n");
	yaml_parser_set_input_file(&parser, fileIn);

	while(!endEvent){	
		/* Get the next event. */
		yaml_parser_parse(&parser, &input_event);
		
		/* Check if this is the stream end. */
		if(input_event.type == YAML_STREAM_END_EVENT)
			endEvent = 1;
		if(input_event.type == YAML_SCALAR_EVENT){
		
			if(!strcmp((const char *)input_event.data.scalar.value, "FileName")){
				yaml_parser_parse(&parser, &input_event);
				hkl3dConfig.fileName = (const char *)input_event.data.scalar.value;
				this->_hkl3dConfigs.push_back(hkl3dConfig);
				i++;
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Id")){	
				yaml_parser_parse(&parser, &input_event);
				objectConfig.id = atof((const char*)input_event.data.scalar.value);
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Name")){	
				yaml_parser_parse(&parser, &input_event);
				objectConfig.name = (const char*)input_event.data.scalar.value;
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Transformation")){
				int j;

				yaml_parser_parse(&parser, &input_event);
				for(j=0; j<16; j++){
					yaml_parser_parse(&parser, &input_event);
					if(input_event.type == YAML_SCALAR_EVENT)
						objectConfig.transformation[j] = atof((const char*)input_event.data.scalar.value);
				}	
			}

			if(!strcmp((const char *)input_event.data.scalar.value, "Hide")){
				yaml_parser_parse(&parser, &input_event);
			
				objectConfig.hide = strcmp((const char *)input_event.data.scalar.value, "no");
				this->_hkl3dConfigs[i-1].objects.push_back(objectConfig);
			}	
		}	
	}
	yaml_event_delete(&input_event);
    	yaml_parser_delete(&parser);
	fclose(fileIn);
}

void Hkl3D::emitObjectInYamlFile(void)
{
	int i;

	for(i=0; i<this->_hkl3dConfigs.size(); i++)
	{
		int j;
		char number[64];
		int properties1, key1, value1,seq0;
		int root;
		time_t now;
		yaml_emitter_t emitter;
		yaml_document_t output_document;
		yaml_event_t output_event;
		FILE * fileOut;

		memset(&emitter, 0, sizeof(emitter));
		memset(&output_document, 0, sizeof(output_document));
		memset(&output_event, 0, sizeof(output_event));
	
		if (!yaml_emitter_initialize(&emitter)) 
			fprintf(stderr, "Could not inialize the emitter object\n");
	 
		/* Set the emitter parameters */
		fileOut = fopen("config1.yaml", "a+");
		yaml_emitter_set_output_file(&emitter, fileOut);
		yaml_emitter_open(&emitter);
	
		/* Create an output_document object */
		if (!yaml_document_initialize(&output_document, NULL, NULL, NULL, 0, 0))
			fprintf(stderr, "Could not create a output_document object\n");
	
		/* Create the root of the config file */ 
		time(&now);
		root = yaml_document_add_sequence(&output_document,
						  (yaml_char_t*)ctime(&now),
						  YAML_BLOCK_SEQUENCE_STYLE);

		/* create the property of the root sequence */
		properties1 = yaml_document_add_mapping(&output_document,
							(yaml_char_t*)"tag:yaml.org,2002:map",
							YAML_BLOCK_MAPPING_STYLE);

		yaml_document_append_sequence_item(&output_document, root, properties1);
	
		/* add the map key1 : value1 to the property */
		key1 = yaml_document_add_scalar(&output_document,
						NULL,
						(yaml_char_t*)"FileName", 
						-1, 
						YAML_PLAIN_SCALAR_STYLE);
		value1 = yaml_document_add_scalar(&output_document,
						  NULL,
						  (yaml_char_t*)_hkl3dConfigs[i].fileName,
						  -1, 
						  YAML_PLAIN_SCALAR_STYLE);
		yaml_document_append_mapping_pair(&output_document, properties1, key1, value1);

		/* add the map key1 : seq0 to the first property */
		key1 = yaml_document_add_scalar(&output_document,
						NULL,
						(yaml_char_t*)"Objects",
						-1,
						YAML_PLAIN_SCALAR_STYLE);
		/* create the sequence of objects */
		seq0 = yaml_document_add_sequence(&output_document,
						  (yaml_char_t*)"tag:yaml.org,2002:seq",
						  YAML_BLOCK_SEQUENCE_STYLE);
		for(j=0; j<this->_hkl3dConfigs[i].objects.size(); j++){
			int k;
			int properties;
			int key;
			int value;
			int seq1;
		
			properties = yaml_document_add_mapping(&output_document,
							       (yaml_char_t*)"tag:yaml.org,2002:map",
							       YAML_BLOCK_MAPPING_STYLE);
			yaml_document_append_sequence_item(&output_document,seq0, properties);

			key = yaml_document_add_scalar(&output_document, 
						       NULL,
						       (yaml_char_t*)"Id", -1, 
						       YAML_PLAIN_SCALAR_STYLE);
		
			sprintf(number, "%d",_hkl3dConfigs[i].objects[j].id);
			value = yaml_document_add_scalar(&output_document,
							 NULL,
							 (yaml_char_t*)number,
							 -1, 
							 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);

			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t*)"Name", 
						       -1, 
						       YAML_PLAIN_SCALAR_STYLE);
			value = yaml_document_add_scalar(&output_document,
							 NULL,
							 (yaml_char_t*)_hkl3dConfigs[i].objects[j].name,
							 -1, 
							 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);

			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t*)"Transformation",
						       -1,
						       YAML_PLAIN_SCALAR_STYLE);
			seq1 = yaml_document_add_sequence(&output_document, 
							  (yaml_char_t*)"tag:yaml.org,2002:seq",
							  YAML_FLOW_SEQUENCE_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties, key, seq1);
			for(k=0; k<16; k++){
				sprintf(number, "%f",_hkl3dConfigs[i].objects[j].transformation[k]);
				value = yaml_document_add_scalar(&output_document,
								NULL,
								(yaml_char_t*)number, 
								-1, 	
								YAML_PLAIN_SCALAR_STYLE);
				yaml_document_append_sequence_item(&output_document,seq1,value);
			}
	
			key = yaml_document_add_scalar(&output_document,
						       NULL,
						       (yaml_char_t*)"Hide",
						       -1,
						       YAML_PLAIN_SCALAR_STYLE);
			if(_hkl3dConfigs[i].objects[j].hide)
				value = yaml_document_add_scalar(&output_document,
								 NULL,
								 (yaml_char_t*)"yes",
								 -1,
								 YAML_PLAIN_SCALAR_STYLE);
			else
				value = yaml_document_add_scalar(&output_document,
								 NULL,
								 (yaml_char_t*)"no",
								 -1,
								 YAML_PLAIN_SCALAR_STYLE);
			yaml_document_append_mapping_pair(&output_document,properties,key,value);
		}
		yaml_document_append_mapping_pair(&output_document, properties1, key1, seq0);

		/* flush the document */
		yaml_emitter_dump(&emitter, &output_document);
		fclose(fileOut);

		yaml_document_delete(&output_document);
		yaml_emitter_delete(&emitter);
	}
}

void Hkl3D::applyTransformations(void)
{
	int i;
	int k;
	struct timeval debut, fin, dt;

	// set the right transformation of each objects and get numbers
	gettimeofday(&debut, NULL);
	for(i=0;i<HKL_LIST_LEN(this->_geometry->holders);i++){
		size_t j;
		btQuaternion btQ(0, 0, 0, 1);

		size_t len = HKL_LIST_LEN(this->_geometry->holders[i].idx);
		for(j=0; j<len; j++){
			size_t k;
			size_t idx = this->_geometry->holders[i].idx[j];
			HklAxis *axis = &this->_geometry->axes[idx];
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
	int k;
	bool res = true;
	struct timeval debut, fin, dt;
	int numManifolds;

	//apply geometry transformation
	this->applyTransformations();
	// perform the collision detection and get numbers
	gettimeofday(&debut, NULL);
	if(_btCollisionWorld){
		_btCollisionWorld->performDiscreteCollisionDetection();
		_btCollisionWorld->updateAabbs();
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &dt);
	fprintf(stdout, " collision (%f ms)", dt.tv_sec*1000.+dt.tv_usec/1000.);
	
	numManifolds = _btCollisionWorld->getDispatcher()->getNumManifolds();
	for(k=0;k<_hkl3dObjects.size();k++)
		_hkl3dObjects[k].is_colliding=false;
	for(k=0;k<_hkl3dObjects.size();k++){
		ContactSensorCallback callback(*_hkl3dObjects[k].collisionObject,*this,k);
		_btCollisionWorld->contactTest(_hkl3dObjects[k].collisionObject,callback);		
	}
	fprintf(stdout, " manifolds (%d)\n", numManifolds);

	return numManifolds == 0;
}

/*
 * Check that for each moving part in the hkl library there is a
 * corresponding model in the 3d model file.
 *!!!!!!!!!!!! Missing error handling in isModelCompatiblewithGeometry method
 */
bool Hkl3D::isModelFileCompatibleWithGeometry(void)
{
	size_t i;
	bool found;

	for(i=0; i<_len; ++i){
		const char *name;
		GSList *objects;
		std::string resp;

		name = ((HklParameter*)(&_geometry->axes[i]))->name;
		objects = _model->objects;
		found = false;
		while(objects || !found){	  
			G3DObject *object;

			object = (G3DObject *)objects->data;
			if(!strcmp(object->name, name)){
				found = true;
				fprintf(stdout, "%s found in the 3d model file\n", object->name);
			}
			objects = g_slist_next(objects);
		}
		if(found == false){
			resp = name;
			resp += " not found in model file\n";
		}
	}
	return found;	
}
/*
 * load the model in the hkl3d structure
 */
void Hkl3D::loadModelInCollisionWorld(G3DModel * model)
{
	GSList *objects; // lets iterate from the first object.
	int maxSerializeBufferSize = 1024*1024*5;
	btDefaultSerializer* serializer = new btDefaultSerializer(maxSerializeBufferSize);
	serializer->startSerialization();
	// first initialize the _movingBtCollisionObjects and _movingG3DObjects with the right len.
	_movingBtCollisionObjects.resize(_len);
	_movingG3DObjects.resize(_len);
	// for each object in the model file do the g3d -> bullet conversion
	FILE* fileBuffer = fopen("../../data/model.bullet","wb");
	objects = model->objects;
	while(objects){
		G3DObject *object;
		G3DMaterial *material;
		int j=0;
		object = (G3DObject*)objects->data;
		if(object->vertex_count){
			float *vertex;
			int idx;

			GSList *faces;
			
			btCollisionShape* shape;
			btCollisionObject *btObject;
			btTriangleMesh *trimesh;
			
			trimesh = new btTriangleMesh();			
			trimesh->preallocateVertices(object->vertex_count);
			faces = object->faces;
			vertex = object->vertex_data;

			// extract the color from the first face
			// usefull for the demo
			material = ((G3DFace *)faces->data)->material;
			fprintf(stdout, "colors: %f %f %f %f\n",
				material->r, material->g, material->b, material->a);
			while(faces){
				G3DFace * face;
				
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
			idx = hkl_geometry_get_axis_idx_by_name(_geometry, object->name);
			if (idx >= 0){	
				shape=new btGImpactMeshShape(trimesh);
				shape->setMargin(btScalar(0));
				shape->setLocalScaling(btVector3(1,1,1));
				(static_cast<btGImpactMeshShape*>(shape))->updateBound();
				(static_cast<btGImpactMeshShape*>(shape))->postUpdate();
	
				// create the Object and add the shape
				btObject = new btCollisionObject();
				btObject->setCollisionShape(shape);
				btObject->activate(true);
				
				//fill movingCollisionObject and movingG3DObjects vectors for transformations
				_movingBtCollisionObjects[idx].push_back(btObject);
				object->transformation = g_new0(G3DTransformation, 1);
				_movingG3DObjects[idx].push_back(object);
			}
			else{
				shape=new btBvhTriangleMeshShape (trimesh,true);
				shape->setMargin(btScalar(0));
				shape->setLocalScaling(btVector3(1,1,1));
				
				// create the Object and add the shape
				btObject = new btCollisionObject();
				btObject->setCollisionShape(shape);
				btObject->activate(true);
			}	
			serializer->registerNameForPointer(shape,object->name);
			shape->serializeSingleShape(serializer);

			// create hkl3d object structure.
			HKL3DObject hkl3dObject;
			hkl3dObject.collisionObject = btObject;
			hkl3dObject.gObject=object;
			hkl3dObject.collisionShape=shape;
			hkl3dObject.meshes=trimesh;
			hkl3dObject.color=new btVector3(material->r, material->g, material->b);
			hkl3dObject.name=object->name;
			hkl3dObject.is_colliding=false;

			// insert collision Object in collision world
			_btCollisionWorld->addCollisionObject(hkl3dObject.collisionObject);
			
			// remembers objects to avoid memory leak			
			_hkl3dObjects.push_back(hkl3dObject);	 
		}
		objects = g_slist_next(objects);
	}
	serializer->finishSerialization();			
	fwrite(serializer->getBufferPointer(),serializer->getCurrentBufferSize(),1,fileBuffer);	
	fclose(fileBuffer);
}
#ifndef SERIALIZE_TO_DISK
void Hkl3D::importFromBulletFile(void)
{
	int k;
	int idx;
	btCompoundShape* shape;
	//btGImpactCompoundShape* shape;
	btCollisionObject* btObject;
	btBulletWorldImporter import(0);
	// first initialize the _movingBtCollisionObjects and _movingG3DObjects with the right len.
	_movingBtCollisionObjects.resize(_len);
	_movingG3DObjects.resize(_len);
	if (import.loadFile("../../data/model.bullet"))
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
				hkl3dObject.gObject=object;
				hkl3dObject.collisionObject = btObject;
				hkl3dObject.collisionShape=shape;
				hkl3dObject.color=new btVector3(material->r, material->g, material->b);
				hkl3dObject.is_colliding=false;
			}
			// remembers objects to avoid memory leak			
			_hkl3dObjects.push_back(hkl3dObject);
		}
	}
	else
		std::cout<<"Error while loading bullet file"<<std::endl;
}
#endif
