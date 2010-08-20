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

// forward declaration due to bullet static linking
struct btCollisionObject;
struct btCollisionWorld;
struct btCollisionConfiguration;
struct btBroadphaseInterface;
struct btCollisionDispatcher;
struct btCollisionShape;
struct btVector3;
struct btTriangleMesh;

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
		struct btCollisionObject *btObject;
		G3DObject *g3dObject;
		struct btCollisionShape *btShape;
		struct btTriangleMesh *meshes;
		struct btVector3 *color;
		int is_colliding;
		int hide;
		int added;
		int selected;
		int movable;
		char *axis_name;
		float transformation[16];
	};

	extern void hkl3d_object_fprintf(FILE *f, const struct Hkl3DObject *self);

	/***************/
	/* HKL3DConfig */
	/***************/

	struct Hkl3DConfig
	{
		char *filename;	
		struct Hkl3DObject *objects;
		int len;
	};

	extern void hkl3d_config_fprintf(FILE *f, const struct Hkl3DConfig *self);

	/****************/
	/* HKL3DConfigs */
	/****************/

	struct Hkl3DConfigs
	{
		struct Hkl3DConfig *configs;
		int len;
	};

	extern void hkl3d_configs_fprintf(FILE *f, const struct Hkl3DConfigs *self);

	/*************/
	/* HKL3DAxis */
	/*************/

	struct Hkl3DAxis
	{
		struct Hkl3DObject **objects; /* connected object */
		int len;
	};

	/*****************/
	/* HKL3DGeometry */
	/*****************/

	struct Hkl3DGeometry
	{
		struct Hkl3DAxis **axes;
		int len;
	};

	/*********/
	/* HKL3D */
	/*********/

	struct Hkl3D
	{
		char const *filename; /* config filename */
		HklGeometry *geometry; /* do not own this object */
		G3DModel *model;
		struct Hkl3DStats stats;
		struct Hkl3DConfigs *configs;
		struct Hkl3DGeometry *movingObjects;

		size_t _len;
		G3DContext *_context;
		struct btCollisionConfiguration *_btCollisionConfiguration;
		struct btBroadphaseInterface *_btBroadphase;
		struct btCollisionWorld *_btWorld;
		struct btCollisionDispatcher *_btDispatcher;
#ifdef USE_PARALLEL_DISPATCHER
		struct btThreadSupportInterface *_btThreadSupportInterface;
#endif
	};

	extern struct Hkl3D* hkl3d_new(const char *filename, HklGeometry *geometry);
	extern void hkl3d_free(struct Hkl3D *self);

	extern bool hkl3d_is_colliding(struct Hkl3D *self);
	extern void hkl3d_load_config(struct Hkl3D *self, const char *filename);
	extern void hkl3d_save_config(struct Hkl3D *self, const char *filename);
	extern struct Hkl3DConfig *hkl3d_add_model_from_file(struct Hkl3D *self,
							     const char *filename, const char *directory);


	extern void hkl3d_connect_all_axes(struct Hkl3D *self);
	extern void hkl3d_hide_object(struct Hkl3D *self, Hkl3DObject *object, bool hide);
	extern void hkl3d_get_bounding_boxes(struct Hkl3D *self,
					     struct btVector3 & min, btVector3 & max);
	extern int hkl3d_get_nb_manifolds(struct Hkl3D *self);
	extern int hkl3d_get_nb_contacts(struct Hkl3D *self, int manifold);
	extern void hkl3d_get_collision_coordinates(struct Hkl3D *self, int manifold, int contact,
						    double *xa, double *ya, double *za,
						    double *xb, double *yb, double *zb);
	extern void hkl3d_connect_object_to_axis(struct Hkl3D *self,
						 struct Hkl3DObject *object, const char *name);

#ifdef __cplusplus
}
#endif

#endif

