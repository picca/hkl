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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GEOMETRY_PRIVATE_H__
#define __HKL_GEOMETRY_PRIVATE_H__

#include <stddef.h>                     // for size_t
#include <stdio.h>                      // for FILE
#include "hkl-parameter-private.h"      // for darray_parameter
#include "hkl-quaternion-private.h"     // for _HklQuaternion
#include "hkl-source-private.h"         // for HklSource
#include "hkl-vector-private.h"         // for HklQuaternion
#include "hkl.h"                        // for HklGeometry, etc
#include "hkl/ccan/darray/darray.h"     // for darray
#include "hkl/ccan/list/list.h"

G_BEGIN_DECLS

typedef struct _HklHolder HklHolder;
typedef void (* HklGeometryListMultiplyFunction) (HklGeometryList *self,
						  HklGeometryListItem *item);

typedef darray(HklHolder *) darray_holder;

struct HklHolderConfig {
	int gc;
	size_t *idx;
	size_t len;
};

struct _HklHolder {
	struct HklHolderConfig *config;
	HklGeometry *geometry;
	HklQuaternion q;
};

struct _HklGeometry
{
	const HklFactory *factory;
	HklSource source;
	darray_parameter axes;
	darray_holder holders;
};

#define HKL_GEOMETRY_ERROR hkl_geometry_error_quark ()

static GQuark hkl_geometry_error_quark (void)
{
	return g_quark_from_static_string ("hkl-geometry-error-quark");
}

typedef enum {
	HKL_GEOMETRY_ERROR_AXIS_GET, /* can not get the axis */
	HKL_GEOMETRY_ERROR_AXIS_SET, /* can not set the axis */
} HklGeometryError;

struct _HklGeometryList
{
	HklGeometryListMultiplyFunction multiply;
	struct list_head items;
	size_t n_items;
};

struct _HklGeometryListItem
{
	struct list_node list;
	HklGeometry *geometry;
};

/*************/
/* HklHolder */
/*************/

extern HklParameter *hkl_holder_add_rotation_axis(HklHolder *self,
						  char const *name, double x, double y, double z);

/***************/
/* HklGeometry */
/***************/

extern HklGeometry *hkl_geometry_new(const HklFactory *factory);

extern int hkl_geometry_init_geometry(HklGeometry *self,
				      const HklGeometry *src);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);

extern void hkl_geometry_update(HklGeometry *self);

extern int hkl_geometry_get_axis_idx_by_name(const HklGeometry *self,
					     const char *name);

/* internally require do not use the hkl_geometry_axis_get */
extern HklParameter *hkl_geometry_get_axis_by_name(HklGeometry *self,
						   const char *name);

extern double hkl_geometry_distance(const HklGeometry *self,
				    const HklGeometry *ref);

extern double hkl_geometry_distance_orthodromic(const HklGeometry *self,
						const HklGeometry *ref);

extern int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self,
							 const HklGeometry *ref);

extern int hkl_geometry_is_valid(const HklGeometry *self);

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void);

extern HklGeometryList *hkl_geometry_list_new_copy(const HklGeometryList *self);

extern void hkl_geometry_list_add(HklGeometryList *self, HklGeometry *geometry);

extern void hkl_geometry_list_reset(HklGeometryList *self);

extern void hkl_geometry_list_sort(HklGeometryList *self, HklGeometry *ref);

extern void hkl_geometry_list_fprintf(FILE *f, const HklGeometryList *self);

extern void hkl_geometry_list_multiply(HklGeometryList *self);

extern void hkl_geometry_list_multiply_from_range(HklGeometryList *self);

extern void hkl_geometry_list_remove_invalid(HklGeometryList *self);

/***********************/
/* HklGeometryListItem */
/***********************/

extern HklGeometryListItem *hkl_geometry_list_item_new(const HklGeometry *geometry);

extern HklGeometryListItem *hkl_geometry_list_item_new_copy(const HklGeometryListItem *self);

extern void hkl_geometry_list_item_free(HklGeometryListItem *self);

G_END_DECLS

#endif /* __HKL_GEOMETRY_PRIVATE_H__ */
