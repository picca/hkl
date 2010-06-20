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
#include <vector>
#include <g3d/types.h>

// forward declaration due to bullet static linking
class btCollisionObject;
class btCollisionWorld;
class btCollisionConfiguration;
class btBroadphaseInterface;
class btCollisionDispatcher;
class btCollisionShape;
class btVector3;
class btTriangleMesh;

struct Hkl3DObject
{
	int id;
	const char *name;
	btCollisionObject *btObject;
	G3DObject *g3dObject;
	btCollisionShape *btShape;
	btTriangleMesh *meshes;
	btVector3 *color;
	bool is_colliding;
	bool hide;
	bool added;
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
	void load_config(const char *filename);
	void save_config(const char * filename);
	Hkl3DConfig *add_model_from_file(const char *filename, const char *directory);

	HklGeometry * _geometry; // do not own this object
	btCollisionWorld *_btWorld;
	btCollisionDispatcher *_btDispatcher;
	G3DModel *_model;
	std::vector<Hkl3DConfig> _hkl3dConfigs;
#ifdef USE_PARALLEL_DISPATCHER
	class btThreadSupportInterface *_btThreadSupportInterface;
#endif
#ifdef SERIALIZE_TO_DISK
	void importFromBulletFile(const char *filename);
#endif

protected:
	virtual void apply_transformations(void);

private:
	size_t _len;
	G3DContext *_context;
	btCollisionConfiguration *_btCollisionConfiguration;
	btBroadphaseInterface *_btBroadphase;
	std::vector<std::vector<btCollisionObject *> > _movingBtCollisionObjects;
	std::vector<std::vector<G3DObject *> > _movingG3DObjects;

	void init_internals(G3DModel *model, const char *filename);
};
#endif

