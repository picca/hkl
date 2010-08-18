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

/***************/
/* static part */
/***************/

static float identity[] = {1, 0, 0, 0,
			   0, 1, 0, 0,
			   0, 0, 1 ,0,
			   0, 0, 0, 1};

/***************/
/* Hkl3DObject */
/***************/

static void hkl3d_object_init(struct Hkl3DObject *self, G3DObject *object, btCollisionShape *shape,
			      btCollisionObject *btObject, btTriangleMesh *trimesh, int id,
			      const char* filename)
{
	int i;
	GSList *faces;
	G3DMaterial* material;

	// extract the color from the first face
	faces = object->faces;
	material = ((G3DFace *)faces->data)->material;

	// fill the hkl3d object structure.
	self->id = id;
	self->axis_name = strdup(object->name);
	self->btObject = btObject;
	self->g3dObject = object;
	self->btShape = shape;
	self->meshes = trimesh;
	self->color = new btVector3(material->r, material->g, material->b);
	self->hide = object->hide;
	self->added = false;
	self->selected = false;
	self->movable = false;
	self->filename = filename;

	/*
	 * if the object already contain a transformation set the Hkl3DObject
	 * transformation with this transformation. Otherwise set it with the
	 * identity
	 */
	if(object->transformation){
		for(i=0; i<16; i++)
			self->transformation[i] = object->transformation->matrix[i];
	}else{
		/* create one as we requiered it to apply our transformations */
		object->transformation = g_new0(G3DTransformation, 1);
		for(i=0; i<16; i++){
			self->transformation[i] = identity[i];
			object->transformation->matrix[i] = identity[i];
		}
	}

	self->is_colliding = false;
}

static void hkl3d_object_release(struct Hkl3DObject *self)
{
	if(!self)
		return;

	if(self->axis_name){
		free(self->axis_name);
		self->axis_name = NULL;
	}
	if(self->meshes){
		delete self->meshes;
		self->meshes = NULL;
	}
	if(self->btShape){
		delete self->btShape;
		self->btShape = NULL;
	}
	if(self->btObject){
		delete self->btObject;
		self->btObject = NULL;
	}
	if(self->color){
		delete self->color;
		self->color = NULL;
	}
}

/**
 * return 0 if identical 1 if not
 */
static int hkl3d_object_cmp(struct Hkl3DObject *object1,
			    struct Hkl3DObject *object2)
{
	if((!strcmp(object1->filename, object2->filename))
	   && (object1->id == object2->id))
		return 0;
	else
		return 1;
}

static void hkl3d_object_set_axis_name(struct Hkl3DObject *self, const char *name)
{
	if(!self || !name || self->axis_name == name)
		return;

	if(self->axis_name)
		free(self->axis_name);
	self->axis_name = strdup(name);
}

void hkl3d_object_fprintf(FILE *f, const struct Hkl3DObject *self)
{
	GSList *faces;
	G3DMaterial* material;

	faces = self->g3dObject->faces;
	material = ((G3DFace *)faces->data)->material;
	fprintf(f, "id : %d\n", self->id);
	fprintf(f, "name : %s (%p)\n", self->axis_name, self->axis_name);
	fprintf(f, "btObject : %p\n", self->btObject);
	fprintf(f, "g3dObject : %p\n", self->g3dObject);
	fprintf(f, "btShape : %p\n", self->btShape);
	fprintf(f, "meshes : %p\n", self->meshes);
	fprintf(f, "color : %f, %f, %f\n", material->r, material->g, material->b);
	fprintf(f, "hide : %d\n", self->hide);
	fprintf(f, "added : %d\n", self->added);
	fprintf(f, "selected : %d\n", self->selected);
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
		shape = dynamic_cast<btGImpactMeshShape*>(new btGImpactMeshShape(trimesh));
		shape->setMargin(btScalar(0));
		shape->setLocalScaling(btVector3(1,1,1));
		/* maybe usefull for softbodies (useless for now) */
		(dynamic_cast<btGImpactMeshShape*>(shape))->postUpdate();
		/* needed for the collision and must be call after the postUpdate (doc) */
		(dynamic_cast<btGImpactMeshShape*>(shape))->updateBound();
	}else{
		shape = dynamic_cast<btBvhTriangleMeshShape*>(new btBvhTriangleMeshShape (trimesh, true));
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

/***************/
/* Hkl3DConfig */
/***************/

static void hkl3d_config_add_object(struct Hkl3DConfig *self, struct Hkl3DObject object)
{
	if(!self)
		return;

	self->objects = (struct Hkl3DObject *)realloc(self->objects, sizeof(struct Hkl3DObject) * (self->len + 1));
	self->objects[self->len++] = object;
}

/**
 * hkl3d_config_release:
 * @config: 
 * @btCollisionWorld: 
 *
 * release the memory of an Hkl3dConfig. It also detach all btObjects from the btCollisionWorld
 **/
static void hkl3d_config_release(struct Hkl3DConfig *config, btCollisionWorld *btWorld)
{
	int i;
	struct Hkl3DObject *object;

	free(config->filename);
	object = &config->objects[0];
	for(i=0; i<config->len; ++i){
		btWorld->removeCollisionObject(object->btObject);
		hkl3d_object_release(object);
		object++;
	}
	free(config->objects);
	config->objects = NULL;
	config->len = 0;
}

void hkl3d_config_fprintf(FILE *f, const struct Hkl3DConfig *self)
{
	int i;
	fprintf(f, "config (%d):\n", self->len);
	for(i=0; i<self->len; ++i)
		hkl3d_object_fprintf(f, &self->objects[i]);
}

/****************/
/* Hkl3DConfigs */
/****************/

static struct Hkl3DConfigs* hkl3d_configs_new(void)
{
	struct Hkl3DConfigs* self = NULL;

	self = (struct Hkl3DConfigs*)malloc(sizeof(struct Hkl3DConfigs));
	if(!self)
		return NULL;

	self->configs = NULL;
	self->len = 0;

	return self;
}

static void hkl3d_configs_free(struct Hkl3DConfigs *self)
{
	if(!self)
		return;
	free(self->configs);
	free(self);
}

static void hkl3d_configs_release(struct Hkl3DConfigs *self, btCollisionWorld *_btWorld)
{
	int i;

	for(i=0; i<self->len; ++i)
		hkl3d_config_release(&self->configs[i], _btWorld);
}

static struct Hkl3DConfig* hkl3d_configs_get_last(struct Hkl3DConfigs *self)
{
	return &self->configs[self->len - 1];
}


static void hkl3d_configs_add_config(struct Hkl3DConfigs *self, struct Hkl3DConfig config)
{
	self->configs = (Hkl3DConfig *)realloc(self->configs, sizeof(struct Hkl3DConfig) * (self->len + 1));
	self->configs[self->len++] = config;
}

void hkl3d_configs_fprintf(FILE *f, const struct Hkl3DConfigs *self)
{
	int i;
	fprintf(f, "configs (%d):\n", self->len);
	for(i=0; i<self->len; ++i)
		hkl3d_config_fprintf(f, &self->configs[i]);
}

/**************/
/* Hkl3DStats */
/**************/

void hkl3d_stats_fprintf(FILE *f, struct Hkl3DStats *self)
{
	fprintf(f, "transformation : %f ms collision : %f ms \n", 
		self->transformation.tv_sec*1000. + self->transformation.tv_usec/1000.,
		self->collision.tv_sec*1000. + self->collision.tv_usec/1000.);
}

/*************/
/* Hkl3DAxis */
/*************/

static struct Hkl3DAxis *hkl3d_axis_new(void)
{
	struct Hkl3DAxis *self = NULL;

	self = HKL_MALLOC(Hkl3DAxis);

	self->objects = NULL;
	self->len = 0;

	return self;
}

static void hkl3d_axis_free(struct Hkl3DAxis *self)
{
	if(!self)
		return;

	free(self->objects);
	free(self);
}

/* should be optimized (useless if the Hkl3DObject had a connection with the Hkl3DAxis */
static void hkl3d_axis_attach_object(struct Hkl3DAxis *self, struct Hkl3DObject *object)
{
	self->objects = (Hkl3DObject **)realloc(self->objects, sizeof(*self->objects) * (self->len + 1));
	self->objects[self->len++] = object;
}

/* should be optimized (useless if the Hkl3DObject had a connection with the Hkl3DAxis */
static void hkl3d_axis_detach_object(struct Hkl3DAxis *self, struct Hkl3DObject *object)
{
	int i;

	for(i=0; i<self->len; ++i)
		if(!hkl3d_object_cmp(self->objects[i], object)){
			self->len--;
			/* move all above objects of 1 position */
			if(i < self->len)
				memmove(&self->objects[i], &self->objects[i+1], sizeof(*self->objects) * (self->len - i));
		}
}

/*****************/
/* Hkl3DGeometry */
/*****************/

static struct Hkl3DGeometry *hkl3d_geometry_new(int n)
{
	int i;
	struct Hkl3DGeometry *self = NULL;

	self = HKL_MALLOC(Hkl3DGeometry);

	self->axes = (Hkl3DAxis **)malloc(n * sizeof(*self->axes));
	self->len = n;

	for(i=0; i<n; ++i)
		self->axes[i] = hkl3d_axis_new();

	return self;	
}

static void hkl3d_geometry_free(struct Hkl3DGeometry *self)
{
	if(!self)
		return;

	if(self->len){
		int i;

		for(i=0; i<self->len; ++i)
			hkl3d_axis_free(self->axes[i]);
		free(self->axes);
	}
}

/*********/
/* HKL3D */
/*********/

/* use for the transparency of colliding objects */
struct ContactSensorCallback : public btCollisionWorld::ContactResultCallback
{
	ContactSensorCallback(btCollisionObject & collisionObject, struct Hkl3DObject & object)
		: btCollisionWorld::ContactResultCallback(),
		  collisionObject(collisionObject),
		  object(object)
		{ }
 
	btCollisionObject & collisionObject;
	struct Hkl3DObject & object;

	virtual btScalar addSingleResult(btManifoldPoint & cp,
					 const btCollisionObject *colObj0, int partId0, int index0,
					 const btCollisionObject *colObj1, int partId1, int index1)
		{
			if(object.is_colliding == false
			   && (colObj0 == &collisionObject
			       || colObj1 == &collisionObject))
				object.is_colliding = true;		
			return 0;
		}
};

/*
 * Initialize the bullet collision environment.
 * create the Hkl3DObjects
 * create the Hkl3DConfigs
 */
static void hkl3d_init_internals(struct Hkl3D *self, G3DModel *model, const char *filename)
{
	struct Hkl3DConfig config = {0};
	GSList *objects; // lets iterate from the first object.

	objects = model->objects;
	while(objects){
		G3DObject *object;

		object = (G3DObject*)objects->data;
		if(object->vertex_count){			
			int id;
			int idx;
			bool moving;			
			btCollisionShape *shape;
			btCollisionObject *btObject;
			btTriangleMesh *trimesh;
			struct Hkl3DObject hkl3dObject = {0};
			
			trimesh = trimesh_from_g3dobject(object);
			idx = hkl_geometry_get_axis_idx_by_name(self->geometry, object->name);
			shape = shape_from_trimesh(trimesh, idx);
			btObject = btObject_from_shape(shape);
			id = g_slist_index(model->objects, object);
			hkl3d_object_init(&hkl3dObject, object, shape, btObject, trimesh, id, filename);

			// insert collision Object in collision world
			self->_btWorld->addCollisionObject(hkl3dObject.btObject);
			hkl3dObject.added = true;
			
			// remembers objects to avoid memory leak
			hkl3d_config_add_object(&config, hkl3dObject);
		}
		objects = g_slist_next(objects);
	}
	
	config.filename = strdup(filename);
	hkl3d_configs_add_config(self->configs, config);
}

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
	this->configs = hkl3d_configs_new();
	this->movingObjects = hkl3d_geometry_new(geometry->len);

	// first initialize the _movingObjects with the right len.
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

	this->filename = filename;
	if (filename)
		this->load_config(filename);
}

Hkl3D::~Hkl3D(void)
{
	int i;
	int len;

	// _remove objects from the collision world and delete all objects and shapes
	hkl3d_configs_release(this->configs, _btWorld);
	hkl3d_configs_free(this->configs);
	hkl3d_geometry_free(this->movingObjects);

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

struct Hkl3DConfig *Hkl3D::add_model_from_file(const char *filename, const char *directory)
{	
	G3DModel * model;
	G3DObject *object;
	G3DMaterial *material;
	char current[PATH_MAX];
	struct Hkl3DConfig *config = NULL;
	int res;

	/* first set the current directory using the directory parameter*/
	getcwd(current, PATH_MAX);
	res = chdir(directory);
	//fprintf(stdout, "changing directory from %s -> %s (%d)\n", current, directory, res);
	model = g3d_model_load_full(_context, filename, 0);
	res = chdir(current);
	//fprintf(stdout, "changing directory from %s <- %s (%d)\n", current, directory, res);

	if(model){
		/* concatenate the added Model with the one from Hkl3D */ 
		this->model->objects = g_slist_concat(this->model->objects, model->objects);
		this->model->materials = g_slist_concat(this->model->materials, model->materials);

		/* update the Hkl3D internals from the model */
		hkl3d_init_internals(this, model, filename);
	
		/* if resp == true there is a problem in the diffractometer model. */
		config = hkl3d_configs_get_last(this->configs);
	}
	return config;
}

void Hkl3D::connect_all_axes(void)
{
	int i;
	int j;

	/* connect use the axes names */
	for(i=0;i<this->configs->len;i++)
		for(j=0;j<this->configs->configs[i].len;j++)
			this->connect_object_to_axis(&this->configs->configs[i].objects[j],
						     this->configs->configs[i].objects[j].axis_name);
}

/* check that the axis name is really available in the Geometry */
/* if aixs name not valid make the object static object->name = NULL */
/* ok so check if the axis was already connected  or not */
/* if already connected check if it was a different axis do the job */
/* if not yet connected do the job */
/* fill movingCollisionObject and movingG3DObjects vectors for transformations */
void Hkl3D::connect_object_to_axis(struct Hkl3DObject *object, const char *name)
{
	bool update = false;
	bool connect = false;
	int idx = hkl_geometry_get_axis_idx_by_name(this->geometry, name);
	if (!object->movable){
		if(idx >= 0){ /* static -> movable */
			update = true;
			connect = true;
			object->movable = true;
		}
	}else{
		if(idx < 0){ /* movable -> static */
			object->movable = false;
			update = true;
			connect = false;
		}else{ /* movable -> movable */
			if(strcmp(object->axis_name, name)){ /* not the same axis */
				update = false;
				connect = true;
				int i = hkl_geometry_get_axis_idx_by_name(this->geometry, object->axis_name);
				struct Hkl3DObject **objects;

				for(int k=0;k<this->movingObjects->axes[i]->len;k++){
					objects = this->movingObjects->axes[i]->objects;
					if(!hkl3d_object_cmp(objects[k], object)){
						hkl3d_axis_detach_object(this->movingObjects->axes[i], object);
						break;	
					}		
				}
			}
		}
	}
	hkl3d_object_set_axis_name(object, name);
	if(update){
		/* first deconnected if already connected with a different axis */
		_btWorld->removeCollisionObject(object->btObject);
		delete object->btObject;
		delete object->btShape;
		object->btShape = shape_from_trimesh(object->meshes,idx);
		object->btObject = btObject_from_shape(object->btShape);
		// insert collision Object in collision world
		_btWorld->addCollisionObject(object->btObject);
		object->added = true;
	}
	if(connect)
		hkl3d_axis_attach_object(this->movingObjects->axes[idx], object);
}

void Hkl3D::load_config(const char *filename)
{
	int j,l;
	int newFile=0;
	int endEvent = 0;
	yaml_parser_t parser;
	yaml_event_t input_event;
	FILE *file;
	char *dirc;
	char *dir;
	struct Hkl3DConfig *config;

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
		/* Get the next event. */
		yaml_parser_parse(&parser, &input_event);
		
		/* Check if this is the stream end. */
		if(input_event.type == YAML_STREAM_END_EVENT)
			endEvent = 1;
		if(input_event.type == YAML_DOCUMENT_START_EVENT){
			j=0;
			/* skip 4 events */
			// DOCUMENT-START
			// SEQUENCE-START
			// MAPPING-START
			// SCALAR fileName key
			for(l=0;l<4;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}
		
			/* the add form file method create a default Hkl3DConfig and add it to the HKL3D */
			/* we just need to update this config with the values from the configuration file */
			config = this->add_model_from_file((const char *)input_event.data.scalar.value, dir);
			/* skip 3 events */
			// SCALAR objects key
			// SEQUENCE-START
			// MAPPING-START
			for(l=0;l<3;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}
		}
		
		if((input_event.type==YAML_MAPPING_START_EVENT)&& config){
			j++;
			/* skip 2 events */
			// MAPPING-START
			// SCALAR iD key
			for(l=0;l<2;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* get the object id */
			config->objects[j-1].id = atoi((const char *)input_event.data.scalar.value);

			/* skip 2 more events */
			// SCALAR valueId
			// SCALAR Name key
			for(l=0;l<2;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* get the name of the object from the config file */
			hkl3d_object_set_axis_name(&config->objects[j-1], (const char *)input_event.data.scalar.value);
			/*  skip 3 events */
			// SCALAR NameValue
			// SCALAR transformation key
			// SEQUENCE-START
			for(l=0;l<3;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* get the 16 values of the transformation */
			for(l=0;l<16;l++){
				config->objects[j-1].transformation[l] = atof((const char *)input_event.data.scalar.value);
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* skip 2 events */
			// SEQUENCE-END
			// SCALAR hide key
			for(l=0;l<2;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* get the hide value */
			config->objects[j-1].hide = strcmp((const char *)input_event.data.scalar.value, "no");	
			config->objects[j-1].g3dObject->hide = config->objects[j-1].hide;
		}
		yaml_event_delete(&input_event);
	}

	free(dirc);
    	yaml_parser_delete(&parser);
	fclose(file);

	/* now that everythings goes fine we can save the filename */
	this->filename = filename;

	this->connect_all_axes();
}

void Hkl3D::save_config(const char *filename)
{
	int i;

	for(i=0; i<this->configs->len; i++){
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
		if(!file){
			fprintf(stderr, "Could not open the config file %s to save\n", filename);
			return;
		}
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
						  (yaml_char_t *)this->configs->configs[i].filename,
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
		for(j=0; j<this->configs[i].len; j++){
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
		
			sprintf(number, "%d", this->configs->configs[i].objects[j].id);
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
							 (yaml_char_t *)this->configs->configs[i].objects[j].axis_name,
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
				sprintf(number, "%f", this->configs->configs[i].objects[j].transformation[k]);
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
			if(this->configs->configs[i].objects[j].hide)
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
	struct timeval debut, fin;

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
			
			// conversion beetween hkl -> bullet coordinates
			btQ *= btQuaternion(-axis->q.data[1],
					    axis->q.data[3],
					    axis->q.data[2],
					    axis->q.data[0]);

			// move each object connected to that hkl Axis.
			for(k=0; k<this->movingObjects->axes[idx]->len; ++k){
				this->movingObjects->axes[idx]->objects[k]->btObject->getWorldTransform().setRotation(btQ);
				this->movingObjects->axes[idx]->objects[k]->btObject->getWorldTransform().getOpenGLMatrix( G3DM );
				memcpy(this->movingObjects->axes[idx]->objects[k]->g3dObject->transformation->matrix, &G3DM[0], sizeof(G3DM));
			}

		}
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &this->stats.transformation);
}

/**
 * Hkl3D::hide_object:
 *
 * update the visibility of an Hkl3DObject in the bullet world
 * add or remove the object from the _btWorld depending on the hide
 * member of the object.
 **/
void Hkl3D::hide_object(struct Hkl3DObject *object, bool hide)
{
	// first update the G3DObject
	object->hide = hide;
	object->g3dObject->hide = hide;
	if(object->hide){
		if (object->added){
			_btWorld->removeCollisionObject(object->btObject);
			object->added = false;
		}
	}else{
		if(!object->added){
			_btWorld->addCollisionObject(object->btObject);
			object->added = true;
		}
	}
}

bool Hkl3D::is_colliding(void)
{
	int i;
	int j;
	bool res = true;
	int numManifolds;
	struct timeval debut, fin;

	//apply geometry transformation
	this->apply_transformations();
	// perform the collision detection and get numbers
	gettimeofday(&debut, NULL);
	if(_btWorld){
		_btWorld->performDiscreteCollisionDetection();
		_btWorld->updateAabbs();
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &this->stats.collision);
	
	numManifolds = _btWorld->getDispatcher()->getNumManifolds();

	/* reset all the collisions */
	for(i=0; i<this->configs->len; i++)
		for(j=0; j<this->configs[i].len; j++)
			this->configs->configs[i].objects[j].is_colliding = false;

	/* check all the collisions */
	for(i=0; i<this->configs->len; i++)
		for(j=0; j<this->configs[i].len; j++){
			struct Hkl3DObject & object = this->configs->configs[i].objects[j];
			ContactSensorCallback callback(*object.btObject, object);
			_btWorld->contactTest(object.btObject, callback);
		}		

	return numManifolds != 0;
}

/**
 * Hkl3D::get_bounding_boxes:
 * @min: 
 * @max: 
 *
 * get the bounding boxes of the current world from the bullet internals.
 **/
void Hkl3D::get_bounding_boxes(btVector3 & min, btVector3 & max)
{
	_btWorld->getBroadphase()->getBroadphaseAabb(min, max);
}

int Hkl3D::get_nb_manifolds(void)
{
	return _btDispatcher->getNumManifolds();
}

int Hkl3D::get_nb_contacts(int manifold)
{
	return _btDispatcher->getManifoldByIndexInternal(manifold)->getNumContacts();
}

void Hkl3D::get_collision_coordinates(int manifold, int contact,
				      double *xa, double *ya, double *za,
				      double *xb, double *yb, double *zb)
{
	btPersistentManifold *contactManifold;
	btCollisionObject *obA;
	btCollisionObject *obB;

	contactManifold = _btDispatcher->getManifoldByIndexInternal(manifold);
	obA = static_cast<btCollisionObject*>(contactManifold->getBody0());
	obB = static_cast<btCollisionObject*>(contactManifold->getBody1());
	btManifoldPoint & pt = contactManifold->getContactPoint(contact);
	btVector3 ptA = pt.getPositionWorldOnA();
	btVector3 ptB = pt.getPositionWorldOnB();

	*xa = ptA.x();
	*ya = ptA.y();
	*za = ptA.z();
	*xb = ptB.x();
	*yb = ptB.y();
	*zb = ptB.z();
}
