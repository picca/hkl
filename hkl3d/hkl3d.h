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

	typedef struct _Hkl3DStats Hkl3DStats;
	typedef struct _Hkl3DObject Hkl3DObject;
	typedef struct _Hkl3DConfig Hkl3DConfig;
	typedef struct _Hkl3DConfigs Hkl3DConfigs;
	typedef struct _Hkl3DAxis Hkl3DAxis;
	typedef struct _Hkl3DGeometry Hkl3DGeometry;
	typedef struct _Hkl3D Hkl3D;

	/**************/
	/* Hkl3DStats */
	/**************/

	struct _Hkl3DStats
	{
		struct timeval collision;
		struct timeval transformation;
	};

	extern double hkl3d_stats_get_collision_ms(const Hkl3DStats *self);
	extern void hkl3d_stats_fprintf(FILE *f, Hkl3DStats *self);

	/***************/
	/* Hkl3DObject */
	/***************/

	struct _Hkl3DObject
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

	extern void hkl3d_object_fprintf(FILE *f, const Hkl3DObject *self);

	/***************/
	/* HKL3DConfig */
	/***************/

	struct _Hkl3DConfig
	{
		char *filename;	
		Hkl3DObject **objects;
		int len;
	};

	extern void hkl3d_config_fprintf(FILE *f, const Hkl3DConfig *self);

	/****************/
	/* HKL3DConfigs */
	/****************/

	struct _Hkl3DConfigs
	{
		Hkl3DConfig **configs;
		int len;
	};

	extern void hkl3d_configs_fprintf(FILE *f, const Hkl3DConfigs *self);

	/*************/
	/* HKL3DAxis */
	/*************/

	struct _Hkl3DAxis
	{
		Hkl3DObject **objects; /* connected object */
		int len;
	};

	/*****************/
	/* HKL3DGeometry */
	/*****************/

	struct _Hkl3DGeometry
	{
		HklGeometry *geometry; /* weak reference */
		Hkl3DAxis **axes;
		int len;
	};

	/*********/
	/* HKL3D */
	/*********/

	struct _Hkl3D
	{
		char const *filename; /* config filename */
		Hkl3DGeometry *geometry;
		G3DModel *model;
		Hkl3DStats stats;
		Hkl3DConfigs *configs;

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

	extern Hkl3D* hkl3d_new(const char *filename, HklGeometry *geometry);
	extern void hkl3d_free(Hkl3D *self);

	extern int hkl3d_is_colliding(Hkl3D *self);
	extern void hkl3d_load_config(Hkl3D *self, const char *filename);
	extern void hkl3d_save_config(Hkl3D *self, const char *filename);
	extern Hkl3DConfig *hkl3d_add_model_from_file(Hkl3D *self,
						      const char *filename, const char *directory);

	extern void hkl3d_connect_all_axes(Hkl3D *self);
	extern void hkl3d_hide_object(Hkl3D *self, Hkl3DObject *object, int hide);
	extern void hkl3d_remove_object(Hkl3D *self, Hkl3DObject *object);

	extern void hkl3d_get_bounding_boxes(Hkl3D *self,
					     struct btVector3 *min, struct btVector3 *max);
	extern int hkl3d_get_nb_manifolds(Hkl3D *self);
	extern int hkl3d_get_nb_contacts(Hkl3D *self, int manifold);
	extern void hkl3d_get_collision_coordinates(Hkl3D *self, int manifold, int contact,
						    double *xa, double *ya, double *za,
						    double *xb, double *yb, double *zb);
	extern void hkl3d_connect_object_to_axis(Hkl3D *self,
						 Hkl3DObject *object, const char *name);

	extern void hkl3d_fprintf(FILE *f, const Hkl3D *self);

#ifdef __cplusplus
}
#endif

#endif

