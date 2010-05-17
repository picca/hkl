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
#include <iostream>
#include <string.h>
#include <sys/time.h>
#include "hkl3d.h"
#include "btBulletCollisionCommon.h"
#include "BulletCollision/Gimpact/btGImpactCollisionAlgorithm.h"
#include "btGImpactConvexDecompositionShape.h"

#ifdef USE_PARALLEL_DISPATCHER
# include "BulletMultiThreaded/SpuGatheringCollisionDispatcher.h"
# include "BulletMultiThreaded/PlatformDefinitions.h"
# include "BulletMultiThreaded/PosixThreadSupport.h"
# include "BulletMultiThreaded/SpuNarrowPhaseCollisionTask/SpuGatheringCollisionTask.h"
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

	// read the model from the file
	_context = g3d_context_new();
	_model = g3d_model_load_full(_context, filename,G3D_MODEL_SCALE);

	// load model from libg3d into bullet Hullshape.	
	if(this->isModelFileCompatibleWithGeometry() == false)
		throw "Model not compatible with the HklGeometry\n";
	this->loadG3dFaceInBtConvexHullShape();
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

	btVector3 worldAabbMin(-10000,-10000,-10000);
	btVector3 worldAabbMax( 10000, 10000, 10000);

	_btBroadphase = new btAxisSweep3(worldAabbMin, worldAabbMax);

	_btCollisionWorld = new btCollisionWorld(_btDispatcher,
						 _btBroadphase,
						 _btCollisionConfiguration);

	// add all objects to the world
	for(int i=0; i<_hkl3dObjects.size(); i++)
		_btCollisionWorld->addCollisionObject(_hkl3dObjects[i].collisionObject); 

	// if resp == true there is a problem in the diffractometer model.
	bool resp = this->is_colliding();
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
		delete _hkl3dObjects[i].meshes;
		delete _hkl3dObjects[i].collisionShapes;
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
	g3d_context_free(_context);
}

bool Hkl3D::is_colliding(void)
{
	int i;
	int k;
	bool res = true;
	struct timeval debut, fin, dt;
	int numManifolds;

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
void Hkl3D::loadG3dFaceInBtConvexHullShape(void)
{
	GSList *objects; // lets iterate from the first object.

	// first initialize the _movingBtCollisionObjects with the right len.
	_movingBtCollisionObjects.resize(_len);
	_movingG3DObjects.resize(_len);
	// for each object in the model file do the g3d -> bullet conversion
	objects = _model->objects;
	while(objects){
		G3DObject *object;
		G3DMaterial *material;
		int j=0;
		object = (G3DObject*)objects->data;
		if(object->vertex_count){
			GSList *faces;
			btCollisionObject *btObject;
			btTriangleMesh *trimesh;
			float *vertex;
			btGImpactConvexDecompositionShape *shape;
			int idx;
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
			// create the shape
			shape = new btGImpactConvexDecompositionShape (trimesh, btVector3(1.f,1.f,1.f), btScalar(0));
			shape->updateBound();

			// create the Object and add the shape
			btObject = new btCollisionObject();
			btObject->setCollisionShape(shape);
			btObject->activate(true);
			
			// now populate also the moving part
			idx = hkl_geometry_get_axis_idx_by_name(_geometry, object->name);
			if (idx >= 0){
				_movingBtCollisionObjects[idx].push_back(btObject);
				object->transformation = g_new0(G3DTransformation, 1);
				_movingG3DObjects[idx].push_back(object);
			}
			// create hkl3d object structure.
			HKL3DObject hkl3dObject;
			hkl3dObject.collisionObject = btObject;
			hkl3dObject.gObject=object;
			hkl3dObject.collisionShapes=shape;
			hkl3dObject.meshes=trimesh;
			hkl3dObject.color=new btVector3(material->r, material->g, material->b);
			hkl3dObject.is_colliding=false;

			// remembers objects to avoid memory leak			
			_hkl3dObjects.push_back(hkl3dObject);
		}
		objects = g_slist_next(objects);
	}
}
