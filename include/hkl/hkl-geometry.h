/* This file is part of the hkl library.
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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/hkl-source.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-list.h>
#include <hkl/hkl-quaternion.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;
typedef struct _HklGeometry HklGeometry;
typedef struct _HklGeometryList HklGeometryList;
typedef void (* HklGeometryListMultiplyFunction) (HklGeometryList *self,
						  size_t idx);

struct _HklHolder {
	HklGeometry *geometry;
	HKL_LIST(size_t, idx);
	HklQuaternion q;
};

struct _HklGeometry
{
	char const *name;
	HklSource source;
	HKL_LIST(HklAxis, axes);
	HKL_LIST(HklHolder, holders);
};

struct _HklGeometryList
{
	HKL_LIST(HklGeometry *,geometries);
	HklGeometryListMultiplyFunction multiply;
};

/*************/
/* HklHolder */
/*************/

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *self,
					     char const *name, double x, double y, double z);

/***************/
/* HklGeometry */
/***************/

extern HklGeometry *hkl_geometry_new(void);
extern HklGeometry *hkl_geometry_new_copy(HklGeometry const *self);

extern void hkl_geometry_free(HklGeometry *self);

extern void hkl_geometry_init_geometry(HklGeometry *self,
				       HklGeometry const *src);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);

extern void hkl_geometry_update(HklGeometry *self);

extern HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self,
					      char const *name);

extern void hkl_geometry_randomize(HklGeometry *self);

extern int hkl_geometry_set_values_v(HklGeometry *self,
				     size_t len, ...);

extern double hkl_geometry_distance(HklGeometry *self, HklGeometry *geom);

extern double hkl_geometry_distance_orthodromic(HklGeometry *self, HklGeometry *geom);

extern int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self, HklGeometry *ref);

extern void hkl_geometry_fprintf(FILE *file, HklGeometry const *self);

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void);

extern void hkl_geometry_list_free(HklGeometryList *self);

extern void hkl_geometry_list_add(HklGeometryList *self, HklGeometry *geometry);

extern void hkl_geometry_list_reset(HklGeometryList *self);

extern void hkl_geometry_list_sort(HklGeometryList *self, HklGeometry *ref);

extern void hkl_geometry_list_fprintf(FILE *f, HklGeometryList const *self);

extern void hkl_geometry_list_multiply(HklGeometryList *self);

extern void hkl_geometry_list_multiply_from_range(HklGeometryList *self);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
