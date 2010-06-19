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
 *          Oussama SBOUI <oussama.sboui@synchrotron-soleil.fr>
 */

#ifndef __HKL3D_H__
#define __HKL3D_H__

#include <hkl.h>
#include <iostream>
#include <fstream>
#include <g3d/g3d.h>
#include <g3d/types.h>
#include <g3d/quat.h>
#include <g3d/matrix.h>
#include <vector>

// forward declaration due to bullet static linking
class btCollisionObject;
class btCollisionWorld;
class btCollisionConfiguration;
class btBroadphaseInterface;
class btCollisionDispatcher;
class btCollisionShape;
class btVector3;
class btTriangleMesh;

struct Hkl3DObject{
	btCollisionObject *collisionObject;
	G3DObject *gObject;
	btCollisionShape *collisionShape;
	btTriangleMesh *meshes;
	const char *name;
	btVector3 *color;
	bool is_colliding;
	int id;
	bool hide;
	bool AddedInWorldCollision;
	float transformation[16]; 
};

struct Hkl3DConfig
{
	const char *filename;
	std::vector<Hkl3DObject> objects;
};

class Hkl3D
{
public:
	Hkl3D(const char *filename, HklGeometry *geometry);
	~Hkl3D(void);
	bool is_colliding(void);
	void initHkl3d(G3DModel *model, const char *filename);
	virtual void applyTransformations(void);
	void saveConfig(const char * filename);
	Hkl3DConfig *addFromFile(const char *filename, const char *directory);
	void importFromBulletFile(const char *filename);
	void loadConfigFile(const char *filename);
	HklGeometry * _geometry; // do not own this object
	size_t _len;
	G3DModel *_model;
	G3DContext *_context;
	btCollisionWorld *_btCollisionWorld;
	btCollisionDispatcher *_btDispatcher;
	btCollisionConfiguration *_btCollisionConfiguration;
	btBroadphaseInterface *_btBroadphase;
	std::vector<std::vector<btCollisionObject *> > _movingBtCollisionObjects;
	std::vector<std::vector<G3DObject *> > _movingG3DObjects;
	std::vector<Hkl3DConfig> _hkl3dConfigs;
#ifdef USE_PARALLEL_DISPATCHER
	class btThreadSupportInterface *_btThreadSupportInterface;
#endif
};
#endif

