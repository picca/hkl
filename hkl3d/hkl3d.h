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

struct HKL3DCollisionObject{
	btCollisionObject * collisionObject;
	G3DObject * gObject;
};

class Hkl3D
{
public:
	Hkl3D(const char *filename, HklGeometry * geometry);

	~Hkl3D(void);

	bool is_colliding(void);
	bool isModelFileCompatibleWithGeometry(void);
	void loadG3dFaceInBtConvexHullShape(void);

	HklGeometry * _geometry; // do not own this object
	size_t _len;
	G3DContext *_context;
	G3DModel *_model;
	btCollisionDispatcher *_btDispatcher;
	btCollisionConfiguration *_btCollisionConfiguration;
	btBroadphaseInterface *_btBroadphase;
	std::vector<btCollisionObject *> _btCollisionObjects;
	std::vector<btVector3> _colors;
	std::vector<btCollisionShape *>  _btCollisionShapes;
	std::vector<btTriangleMesh *> _btMeshes;
	std::vector<std::vector<btCollisionObject *> > _movingBtCollisionObjects;
	std::vector<std::vector<G3DObject*> > _movingG3DObjects;
	btCollisionWorld *_btCollisionWorld;
	std::vector<HKL3DCollisionObject> _hkl3DCollisionObjectVector;
#ifdef USE_PARALLEL_DISPATCHER
	class btThreadSupportInterface* _btThreadSupportInterface;
#endif
};

#endif

