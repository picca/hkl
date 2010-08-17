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
#include <g3d/types.h>

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

#ifdef __cplusplus
extern "C" {
#endif

	/**************/
	/* Hkl3DStats */
	/**************/

	struct Hkl3DStats
	{
		struct timeval collision;
		struct timeval transformation;
	};

	extern void hkl3d_stats_fprintf(FILE *f, struct Hkl3DStats *self);

	/***************/
	/* Hkl3DObject */
	/***************/

	struct Hkl3DObject
	{
		const char* filename; 
		int id;
		btCollisionObject *btObject;
		G3DObject *g3dObject;
		btCollisionShape *btShape;
		btTriangleMesh *meshes;
		btVector3 *color;
		bool is_colliding;
		bool hide;
		bool added;
		bool selected;
		bool movable;
		const char *axis_name;
		float transformation[16];
	};

	/***************/
	/* HKL3DConfig */
	/***************/

	struct Hkl3DConfig
	{
		char *filename;	
		struct Hkl3DObject *objects;
		int len;
	};

#ifdef __cplusplus
}
#endif


class Hkl3D
{
public:

	char const *filename; /* config filename */
	HklGeometry *geometry; /* do not own this object */
	G3DModel *model;
	std::vector<struct Hkl3DConfig> configs;
	struct Hkl3DStats stats;

	Hkl3D(const char *filename, HklGeometry *geometry);
	~Hkl3D(void);
	bool is_colliding(void);
	void load_config(const char *filename);
	void save_config(const char *filename);
	struct Hkl3DConfig *add_model_from_file(const char *filename, const char *directory);
	
	void connect_object_to_axis(Hkl3DObject *object, const char *name);
	void connect_all_axes(void);
	void hide_object(Hkl3DObject *object, bool hide);
	void get_bounding_boxes(btVector3 & min, btVector3 & max);
	int get_nb_manifolds(void);
	int get_nb_contacts(int manifold);
	void get_collision_coordinates(int manifold, int contact,
				       double *xa, double *ya, double *za,
				       double *xb, double *yb, double *zb);

protected:
	virtual void apply_transformations(void);

private:
	size_t _len;
	G3DContext *_context;
	btCollisionConfiguration *_btCollisionConfiguration;
	btBroadphaseInterface *_btBroadphase;
	btCollisionWorld *_btWorld;
	btCollisionDispatcher *_btDispatcher;
	std::vector<std::vector<Hkl3DObject> > _movingObjects;
#ifdef USE_PARALLEL_DISPATCHER
	class btThreadSupportInterface *_btThreadSupportInterface;
#endif

	void init_internals(G3DModel *model, const char *filename);
};
#endif

