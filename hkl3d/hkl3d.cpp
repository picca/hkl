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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "hkl3d.h"
#include "hkl-geometry-private.h"

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

static void *_hkl3d_malloc(int size, const char *error)
{
	void *tmp;

	tmp = calloc(1, size);
	if(!tmp){
		fprintf(stderr, "%s", error);
		exit(128);
	}

	return tmp;
}
/* malloc method */
#define HKL3D_MALLOC(type) (type *)_hkl3d_malloc(sizeof(type), "Can not allocate memory for a " #type)

/***************/
/* Hkl3DObject */
/***************/

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


static Hkl3DObject *hkl3d_object_new(Hkl3DModel *model, G3DObject *object, int id)
{
	int i;
	GSList *faces;
	G3DMaterial* material;
	Hkl3DObject *self = NULL;

	self = HKL3D_MALLOC(Hkl3DObject);

	/* extract the color from the first face */
	/* this is usefull for the bullet GL draw method */
	faces = object->faces;
	material = ((G3DFace *)faces->data)->material;

	/* fill the hkl3d object structure. */
	self->model = model;
	self->id = id;
	self->axis_name = strdup(object->name);
	self->axis = NULL;
	self->g3d = object;
	self->meshes = trimesh_from_g3dobject(object);
	self->btShape = shape_from_trimesh(self->meshes, false);
	self->btObject = btObject_from_shape(self->btShape);
	self->color = new btVector3(material->r, material->g, material->b);
	self->hide = object->hide;
	self->added = false;
	self->selected = false;
	self->movable = false;

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

	return self;
}

static void hkl3d_object_free(Hkl3DObject *self)
{
	if(!self)
		return;

	/* memory leak in libg3d */
	if(self->g3d && self->g3d->transformation){
		g_free(self->g3d->transformation);
		self->g3d->transformation = NULL;
	}
	if(self->color){
		delete self->color;
		self->color = NULL;
	}
	if(self->btObject){
		delete self->btObject;
		self->btObject = NULL;
	}
	if(self->btShape){
		delete self->btShape;
		self->btShape = NULL;
	}
	if(self->meshes){
		delete self->meshes;
		self->meshes = NULL;
	}
	if(self->axis_name){
		free(self->axis_name);
		self->axis_name = NULL;
	}

	free(self);
}

static void hkl3d_object_set_movable(Hkl3DObject *self, int movable)
{
	if(!self)
		return;

	if(self->movable != movable){
		self->movable = movable;
		delete self->btObject;
		delete self->btShape;
		self->btShape = shape_from_trimesh(self->meshes, movable);
		self->btObject = btObject_from_shape(self->btShape);
	}
}

static void hkl3d_object_set_axis_name(Hkl3DObject *self, const char *name)
{
	if(!self || !name || self->axis_name == name)
		return;

	if(self->axis_name)
		free(self->axis_name);
	self->axis_name = strdup(name);
}

static void matrix_fprintf(FILE *f, const float matrix[])
{
	fprintf(f, "transformation : ");
	for(uint i=0; i<4; ++i){
		if(i)
			fprintf(f, "                 ");
		for(uint j=0; j<4; ++j){
			fprintf(f, " %6.3f", matrix[4 * i + j]);
		}
		fprintf(f, "\n");
	}
}

void hkl3d_object_aabb_get(const Hkl3DObject *self, float from[3], float to[3])
{
	btVector3 min, max;

	self->btShape->getAabb(self->btObject->getWorldTransform(), min, max);

	from[0] = min.getX();
	from[1] = min.getY();
	from[2] = min.getZ();
	to[0] = max.getX();
	to[1] = max.getY();
	to[2] = max.getZ();
}

void hkl3d_object_fprintf(FILE *f, const Hkl3DObject *self)
{
	GSList *faces;
	G3DMaterial* material;

	faces = self->g3d->faces;
	material = ((G3DFace *)faces->data)->material;
	fprintf(f, "id : %d\n", self->id);
	fprintf(f, "name : %s (%p)\n", self->axis_name, self->axis_name);
	fprintf(f, "axis : %p\n", self->axis);
	matrix_fprintf(f, self->transformation);
	fprintf(f, "btObject : %p\n", self->btObject);
	fprintf(f, "g3d : %p\n", self->g3d);
	matrix_fprintf(f, self->g3d->transformation->matrix);
	fprintf(f, "btShape : %p\n", self->btShape);
	fprintf(f, "meshes : %p\n", self->meshes);
	fprintf(f, "color : %f, %f, %f\n", material->r, material->g, material->b);
	fprintf(f, "is_colliding : %d\n", self->is_colliding);
	fprintf(f, "hide : %d\n", self->hide);
	fprintf(f, "added : %d\n", self->added);
	fprintf(f, "selected : %d\n", self->selected);
}

/**************/
/* Hkl3DModel */
/**************/

static Hkl3DModel *hkl3d_model_new(void)
{
	Hkl3DModel *self = NULL;

	self = HKL3D_MALLOC(Hkl3DModel);

	self->filename = NULL;
	self->objects = NULL;
	self->len = 0;
	self->g3d = NULL;

	return self;
}

static void hkl3d_model_free(Hkl3DModel *self)
{
	size_t i;

	if(!self)
		return;

	free(self->filename);
	for(i=0; i<self->len; ++i)
		hkl3d_object_free(self->objects[i]);
	free(self->objects);
	g3d_model_free(self->g3d);
	free(self);
}

static void hkl3d_model_add_object(Hkl3DModel *self, Hkl3DObject *object)
{
	if(!self || !object)
		return;

	self->objects = (typeof(self->objects))realloc(self->objects, sizeof(*self->objects) * (self->len + 1));
	self->objects[self->len++] = object;
}

static void hkl3d_model_delete_object(Hkl3DModel *self, Hkl3DObject *object)
{
	size_t i;

	if(!self || !object)
		return;

	if(self != object->model)
		return;

	/* find the index of the object */
	for(i=0; self->objects[i] != object; ++i);

	hkl3d_object_free(object);
	self->len--;
	/* move all above objects of 1 position */
	if(i < self->len)
		memmove(object, object + 1, sizeof(object) * (self->len - i));
}

void hkl3d_model_fprintf(FILE *f, const Hkl3DModel *self)
{
	fprintf(f, "config (%d):\n", self->len);
	for(size_t i=0; i<self->len; ++i)
		hkl3d_object_fprintf(f, self->objects[i]);
}

/*
 * Initialize the bullet collision environment.
 * create the Hkl3DObjects
 * create the Hkl3DConfig
 */
static Hkl3DModel *hkl3d_model_new_from_file(const char *filename)
{
	G3DModel *model;
	Hkl3DModel *self = NULL;
	GSList *objects; /* lets iterate from the first object. */
	G3DContext *context;

	if(!filename)
		return NULL;

	/* load the model from the file */
	context = g3d_context_new();
	model = g3d_model_load_full(context, filename, 0);
	g3d_context_free(context);
	if(!model)
		return NULL;
	self = hkl3d_model_new();

	self->filename = strdup(filename);
	self->g3d = model;

	/* create all the attached Hkl3DObjects */
	objects = model->objects;
	while(objects){
		G3DObject *object;

		object = (G3DObject*)objects->data;
		if(object->vertex_count){
			int id;
			Hkl3DObject *hkl3dObject;

			id = g_slist_index(model->objects, object);
			hkl3dObject = hkl3d_object_new(self, object, id);

			/* remembers objects to avoid memory leak */
			hkl3d_model_add_object(self, hkl3dObject);
		}
		objects = g_slist_next(objects);
	}
	return self;
}

/***************/
/* Hkl3DConfig */
/***************/

static Hkl3DConfig* hkl3d_config_new(void)
{
	Hkl3DConfig* self = NULL;

	self = (Hkl3DConfig*)malloc(sizeof(Hkl3DConfig));
	if(!self)
		return NULL;

	self->models = NULL;
	self->len = 0;

	return self;
}

static void hkl3d_config_free(Hkl3DConfig *self)
{
	if(!self)
		return;

	for(size_t i=0; i<self->len; ++i)
		hkl3d_model_free(self->models[i]);
	free(self->models);
	free(self);
}

static void hkl3d_config_add_model(Hkl3DConfig *self, Hkl3DModel *model)
{
	self->models = (typeof(self->models))realloc(self->models, sizeof(*self->models) * (self->len + 1));
	self->models[self->len++] = model;
}

void hkl3d_config_fprintf(FILE *f, const Hkl3DConfig *self)
{
	fprintf(f, "models (%d):\n", self->len);
	for(size_t i=0; i<self->len; ++i)
		hkl3d_model_fprintf(f, self->models[i]);
}

/**************/
/* Hkl3DStats */
/**************/

double hkl3d_stats_get_collision_ms(const Hkl3DStats *self)
{
	return self->collision.tv_sec*1000. + self->collision.tv_usec/1000.;
}

void hkl3d_stats_fprintf(FILE *f, const Hkl3DStats *self)
{
	fprintf(f, "transformation : %f ms collision : %f ms \n",
		self->transformation.tv_sec*1000. + self->transformation.tv_usec/1000.,
		hkl3d_stats_get_collision_ms(self));
}

/*************/
/* Hkl3DAxis */
/*************/

static Hkl3DAxis *hkl3d_axis_new(void)
{
	Hkl3DAxis *self = NULL;

	self = HKL3D_MALLOC(Hkl3DAxis);

	self->objects = NULL; /* do not own the objects */
	self->len = 0;

	return self;
}

static void hkl3d_axis_free(Hkl3DAxis *self)
{
	if(!self)
		return;

	free(self->objects);
	free(self);
}

/* should be optimized (useless if the Hkl3DObject had a connection with the Hkl3DAxis */
static void hkl3d_axis_attach_object(Hkl3DAxis *self, Hkl3DObject *object)
{
	object->axis = self;
	self->objects = (Hkl3DObject **)realloc(self->objects, sizeof(*self->objects) * (self->len + 1));
	self->objects[self->len++] = object;
}

/* should be optimized (useless if the Hkl3DObject had a connection with the Hkl3DAxis */
static void hkl3d_axis_detach_object(Hkl3DAxis *self, Hkl3DObject *object)
{
	size_t i;

	if(!self || !object)
		return;

	if(self != object->axis)
		return;

	/* find the index of the object in the object list */
	for(i=0; self->objects[i] != object; ++i);

	object->axis = NULL;
	/* move all above objects of 1 position */
	self->len--;
	if(i < self->len)
		memmove(object, object+1, sizeof(object) * (self->len - i));
}

static void hkl3d_axis_fprintf(FILE *f, const Hkl3DAxis *self)
{
	if(!f || !self)
		return;

	fprintf(f, "Axis len : %d\n", self->len);
	for(size_t i=0; i<self->len; ++i)
		hkl3d_object_fprintf(f, self->objects[i]);
}

/*****************/
/* Hkl3DGeometry */
/*****************/

static Hkl3DGeometry *hkl3d_geometry_new(HklGeometry *geometry)
{
	uint i;
	Hkl3DGeometry *self = NULL;

	self = HKL3D_MALLOC(Hkl3DGeometry);

	self->geometry = geometry;
	self->axes = (Hkl3DAxis **)malloc(darray_size(geometry->axes) * sizeof(*self->axes));

	for(i=0; i<darray_size(geometry->axes); ++i)
		self->axes[i] = hkl3d_axis_new();

	return self;
}

static void hkl3d_geometry_free(Hkl3DGeometry *self)
{
	uint i;

	if(!self)
		return;

	for(i=0; i<darray_size(self->geometry->axes); ++i)
		hkl3d_axis_free(self->axes[i]);
	free(self->axes);
	free(self);
}

static void hkl3d_geometry_apply_transformations(Hkl3DGeometry *self)
{
	HklHolder **holder;

	darray_foreach(holder, self->geometry->holders){
		size_t j;
		btQuaternion btQ(0, 0, 0, 1);

		size_t len = (*holder)->config->len;
		for(j=0; j<len; j++){
			size_t k;
			size_t idx = (*holder)->config->idx[j];
			const HklQuaternion *q = hkl_parameter_quaternion_get(darray_item(self->geometry->axes, idx));
			G3DMatrix G3DM[16];

			/* conversion beetween hkl -> bullet coordinates */
			btQ *= btQuaternion(-q->data[1],
					    q->data[3],
					    q->data[2],
					    q->data[0]);

			/* move each object connected to that hkl Axis. */
			/* apply the quaternion transformation to the bullet object */
			/* use the bullet library to compute the OpenGL matrix */
			/* apply this matrix to the G3DObject for the visualisation */
			for(k=0; k<self->axes[idx]->len; ++k){
				self->axes[idx]->objects[k]->btObject->getWorldTransform().setRotation(btQ);
				self->axes[idx]->objects[k]->btObject->getWorldTransform().getOpenGLMatrix( G3DM );
				memcpy(self->axes[idx]->objects[k]->g3d->transformation->matrix,
				       &G3DM[0], sizeof(G3DM));
			}
		}
	}
}

static void hkl3d_geometry_fprintf(FILE *f, const Hkl3DGeometry *self)
{
	if(!f || !self)
		return;

	fprintf(f, "Hkl3DGeometry : \n");
	hkl_geometry_fprintf(f, self->geometry);
	for(size_t i=0; i<darray_size(self->geometry->axes); ++i)
		hkl3d_axis_fprintf(f, self->axes[i]);
}

static Hkl3DAxis *hkl3d_geometry_axis_get(Hkl3DGeometry *self, const char *name)
{
	for(size_t i=0; i<darray_size(self->geometry->axes); ++i){
		if (!strcmp(hkl_parameter_name_get(darray_item(self->geometry->axes, i)),
			    name))
			return self->axes[i];
	}
	return NULL;
}

/*********/
/* HKL3D */
/*********/

static void hkl3d_apply_transformations(Hkl3D *self)
{
	struct timeval debut, fin;

	/* set the right transformation of each objects and get numbers */
	gettimeofday(&debut, NULL);
	hkl3d_geometry_apply_transformations(self->geometry);
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &self->stats.transformation);
}

void hkl3d_connect_all_axes(Hkl3D *self)
{
	/* connect use the axes names */
	for(size_t i=0;i<self->config->len;i++)
		for(size_t j=0;j<self->config->models[i]->len;j++)
			hkl3d_connect_object_to_axis(self,
						     self->config->models[i]->objects[j],
						     self->config->models[i]->objects[j]->axis_name);
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
Hkl3D *hkl3d_new(const char *filename, HklGeometry *geometry)
{
	Hkl3D *self = NULL;

	self = HKL3D_MALLOC(Hkl3D);

	self->geometry = hkl3d_geometry_new(geometry);
	self->config = hkl3d_config_new();
	self->model= g3d_model_new();

	/* initialize the bullet part */
	self->_btCollisionConfiguration = new btDefaultCollisionConfiguration();

#ifdef USE_PARALLEL_DISPATCHER
	int maxNumOutstandingTasks = 2;
	PosixThreadSupport::ThreadConstructionInfo constructionInfo("collision",
								    processCollisionTask,
								    createCollisionLocalStoreMemory,
								    maxNumOutstandingTasks);
	self->_btThreadSupportInterface = new PosixThreadSupport(constructionInfo);
	self->_btDispatcher = new SpuGatheringCollisionDispatcher(self->_btThreadSupportInterface,
								  maxNumOutstandingTasks,
								  self->_btCollisionConfiguration);
#else
	self->_btDispatcher = new btCollisionDispatcher(self->_btCollisionConfiguration);
#endif
	btGImpactCollisionAlgorithm::registerAlgorithm(self->_btDispatcher);

	btVector3 worldAabbMin(-1000,-1000,-1000);
	btVector3 worldAabbMax( 1000, 1000, 1000);

	self->_btBroadphase = new btAxisSweep3(worldAabbMin, worldAabbMax);

	self->_btWorld = new btCollisionWorld(self->_btDispatcher,
					      self->_btBroadphase,
					      self->_btCollisionConfiguration);

	self->filename = filename;
	if (filename)
		hkl3d_load_config(self, filename);

	return self;
}

void hkl3d_free(Hkl3D *self)
{
	if(!self)
		return;

	/* remove all objects from the collision world */
	for(size_t i=0; i<self->config->len; ++i)
		for(size_t j=0; j<self->config->models[i]->len; ++j)
			if(self->config->models[i]->objects[j]->added)
				self->_btWorld->removeCollisionObject(self->config->models[i]->objects[j]->btObject);

	hkl3d_geometry_free(self->geometry);
	hkl3d_config_free(self->config);

	if (self->_btWorld)
		delete self->_btWorld;
	if (self->_btBroadphase)
		delete self->_btBroadphase;
	if (self->_btDispatcher)
		delete self->_btDispatcher;
#ifdef USE_PARALLEL_DISPATCHER
	if (self->_btThreadSupportInterface){
		/* delete _btThreadSupportInterface; */
		/* _btThreadSupportInterface = 0; */
	}
#endif
	if (self->_btCollisionConfiguration)
		delete self->_btCollisionConfiguration;
	g_free(self->model); /* do not use g3d_model_free as it is juste a container for all config->model */

	free(self);
}

Hkl3DModel *hkl3d_add_model_from_file(Hkl3D *self,
				      const char *filename, const char *directory)
{
	int current;
	Hkl3DModel *model = NULL;
	int res;

	/* first set the current directory using the directory
	 * parameter. Maybe using openat should be a better solution
	 * in the hkl3d_model_new_from_file */
	current = open(".", O_RDONLY);
	if (current < 0)
		return NULL;
	res = chdir(directory);
	if(res < 0)
		goto close_current;

	model = hkl3d_model_new_from_file(filename);
	if(model){
		/* we can not display two different models with the current g3dviewer code */
		/* so concatenate this loaded model with the one of hkl3d */
		self->model->objects = g_slist_concat(self->model->objects, model->g3d->objects);
		self->model->materials = g_slist_concat(self->model->materials, model->g3d->materials);

		/* update the Hkl3D internals from the model */
		hkl3d_config_add_model(self->config, model);
	}
	/* restore the current directory */
	res = fchdir(current);

	return model;

close_current:
	close(current);
	return NULL;
}

/* check that the axis name is really available in the Geometry */
/* if axis name not valid make the object static object->name = NULL */
/* ok so check if the axis was already connected  or not */
/* if already connected check if it was a different axis do the job */
/* if not yet connected do the job */
/* fill movingCollisionObject and movingG3DObjects vectors for transformations */
void hkl3d_connect_object_to_axis(Hkl3D *self, Hkl3DObject *object, const char *name)
{
	Hkl3DAxis *axis3d = hkl3d_geometry_axis_get(self->geometry, name);
	if (!object->movable){
		if(axis3d){ /* static -> movable */
			self->_btWorld->removeCollisionObject(object->btObject);
			hkl3d_object_set_movable(object, true);
			self->_btWorld->addCollisionObject(object->btObject);
			object->added = true;
			hkl3d_axis_attach_object(axis3d, object);
		}
	}else{
		if(!axis3d){ /* movable -> static */
			self->_btWorld->removeCollisionObject(object->btObject);
			hkl3d_object_set_movable(object, false);
			self->_btWorld->addCollisionObject(object->btObject);
			object->added = true;
		}else{ /* movable -> movable */
			if(strcmp(object->axis_name, name)){ /* not the same axis */
				hkl3d_axis_detach_object(object->axis, object);
				hkl3d_axis_attach_object(axis3d, object);
			}
		}
	}
	hkl3d_object_set_axis_name(object, name);
}

void hkl3d_load_config(Hkl3D *self, const char *filename)
{
	int j = 0;
	int l;
	int endEvent = 0;
	yaml_parser_t parser;
	yaml_event_t input_event;
	FILE *file;
	char *dirc;
	char *dir;
	Hkl3DModel *config = NULL;

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

	/* compute the dirname of the config file as all model files */
	/* will be relative to this directory */
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

			/* the add form file method create a default Hkl3DModel and add it to the HKL3D */
			/* we just need to update this config with the values from the configuration file */
			config = hkl3d_add_model_from_file(self, (const char *)input_event.data.scalar.value, dir);
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
			config->objects[j-1]->id = atoi((const char *)input_event.data.scalar.value);

			/* skip 2 more events */
			// SCALAR valueId
			// SCALAR Name key
			for(l=0;l<2;l++){
				yaml_event_delete(&input_event);
				yaml_parser_parse(&parser, &input_event);
			}

			/* get the name of the object from the config file */
			hkl3d_object_set_axis_name(config->objects[j-1], (const char *)input_event.data.scalar.value);
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
				config->objects[j-1]->transformation[l] = atof((const char *)input_event.data.scalar.value);
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
			config->objects[j-1]->hide = strcmp((const char *)input_event.data.scalar.value, "no");
			config->objects[j-1]->g3d->hide = config->objects[j-1]->hide;
		}
		yaml_event_delete(&input_event);
	}

	free(dirc);
	yaml_parser_delete(&parser);
	fclose(file);

	/* now that everythings goes fine we can save the filename */
	self->filename = filename;

	hkl3d_connect_all_axes(self);
}

void hkl3d_save_config(Hkl3D *self, const char *filename)
{
	for(size_t i=0; i<self->config->len; i++){
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
						  (yaml_char_t *)self->config->models[i]->filename,
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
		for(size_t j=0; j<self->config->models[i]->len; j++){
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

			sprintf(number, "%d", self->config->models[i]->objects[j]->id);
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
							 (yaml_char_t *)self->config->models[i]->objects[j]->axis_name,
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
				sprintf(number, "%f", self->config->models[i]->objects[j]->transformation[k]);
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
			if(self->config->models[i]->objects[j]->hide)
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

/**
 * Hkl3D::hide_object:
 *
 * update the visibility of an Hkl3DObject in the bullet world
 * add or remove the object from the _btWorld depending on the hide
 * member of the object.
 **/
void hkl3d_hide_object(Hkl3D *self, Hkl3DObject *object, int hide)
{
	/* first update the G3DObject */
	object->hide = hide;
	object->g3d->hide = hide;
	if(object->hide){
		if (object->added){
			self->_btWorld->removeCollisionObject(object->btObject);
			object->added = false;
		}
	}else{
		if(!object->added){
			self->_btWorld->addCollisionObject(object->btObject);
			object->added = true;
		}
	}
}

/* remove an object from the model */
void hkl3d_remove_object(Hkl3D *self, Hkl3DObject *object)
{
	if(!self || !object)
		return;

	hkl3d_hide_object(self, object, TRUE);
	hkl3d_axis_detach_object(object->axis, object);

	/* now remove the G3DObject from the model */
	self->model->objects = g_slist_remove(self->model->objects, object->g3d);
	hkl3d_model_delete_object(object->model, object);
}

/* use for the transparency of colliding objects */
struct ContactSensorCallback : public btCollisionWorld::ContactResultCallback
{
	ContactSensorCallback(Hkl3DObject *object)
		: btCollisionWorld::ContactResultCallback(),
		  collisionObject(object->btObject),
		  object(object)
		{ }

	btCollisionObject *collisionObject;
	Hkl3DObject *object;

	virtual btScalar addSingleResult(btManifoldPoint & cp,
					 const btCollisionObjectWrapper *colObj0, int partId0, int index0,
					 const btCollisionObjectWrapper *colObj1, int partId1, int index1)
		{
			if(colObj0->m_collisionObject == collisionObject
			   || colObj1->m_collisionObject == collisionObject)
				object->is_colliding = TRUE;
			return 0;
		}
};

int hkl3d_is_colliding(Hkl3D *self)
{
	int numManifolds;
	struct timeval debut, fin;

	/* apply geometry transformation */
	hkl3d_apply_transformations(self);

	/* perform the collision detection and get numbers */
	gettimeofday(&debut, NULL);
	if(self->_btWorld){
		self->_btWorld->performDiscreteCollisionDetection();
		self->_btWorld->updateAabbs();
	}
	gettimeofday(&fin, NULL);
	timersub(&fin, &debut, &self->stats.collision);

	numManifolds = self->_btWorld->getDispatcher()->getNumManifolds();

	/* reset all the collisions */
	for(size_t i=0; i<self->config->len; i++)
		for(size_t j=0; j<self->config->models[i]->len; j++)
			self->config->models[i]->objects[j]->is_colliding = FALSE;

	/* check all the collisions */
	for(size_t i=0; i<self->config->len; i++)
		for(size_t j=0; j<self->config->models[i]->len; j++){
			Hkl3DObject *object = self->config->models[i]->objects[j];
			ContactSensorCallback callback(object);
			self->_btWorld->contactTest(object->btObject, callback);
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
void hkl3d_get_bounding_boxes(Hkl3D *self,
			      struct btVector3 *min, struct btVector3 *max)
{
	self->_btWorld->getBroadphase()->getBroadphaseAabb(*min, *max);
}

int hkl3d_get_nb_manifolds(Hkl3D *self)
{
	return self->_btDispatcher->getNumManifolds();
}

int hkl3d_get_nb_contacts(Hkl3D *self, int manifold)
{
	return self->_btDispatcher->getManifoldByIndexInternal(manifold)->getNumContacts();
}

void hkl3d_get_collision_coordinates(Hkl3D *self, int manifold, int contact,
				     double *xa, double *ya, double *za,
				     double *xb, double *yb, double *zb)
{
	btPersistentManifold *contactManifold;

	contactManifold = self->_btDispatcher->getManifoldByIndexInternal(manifold);
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

void hkl3d_fprintf(FILE *f, const Hkl3D *self)
{

	fprintf(f, "filename : %s\n", self->filename);
	hkl3d_geometry_fprintf(f, self->geometry);
	fprintf(f, "\n");
	fprintf(f, "model : %p\n", self->model);
	hkl3d_stats_fprintf(f, &self->stats);
	hkl3d_config_fprintf(f, self->config);

	fprintf(f, "_btCollisionConfiguration : %p\n", self->_btCollisionConfiguration);
	fprintf(f, "_btBroadphase : %p\n", self->_btBroadphase);
	fprintf(f, "_btWorld : %p\n", self->_btWorld);
	fprintf(f, "_btDispatcher : %p\n", self->_btDispatcher);
#ifdef USE_PARALLEL_DISPATCHER
	fprintf(f, "_btThreadSupportInterface : %p\n", self->_btThreadSupportInterface);
#endif
}
