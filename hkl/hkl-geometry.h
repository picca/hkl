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
 * Copyright (C) 2003-2011 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <ccan/list/list.h>

#include <hkl/hkl-source.h>
#include <hkl/hkl-quaternion.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;
typedef struct _HklGeometryConfig HklGeometryConfig;
typedef struct _HklGeometry HklGeometry;
typedef struct _HklGeometryList HklGeometryList;
typedef struct _HklGeometryListItem HklGeometryListItem;
typedef void (* HklGeometryListMultiplyFunction) (HklGeometryList *self,
						  HklGeometryListItem *item);
typedef enum _HklGeometryType
{
	HKL_GEOMETRY_TYPE_TWOC_VERTICAL,
	HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL,
	HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL,
	HKL_GEOMETRY_TYPE_EULERIAN6C,
	HKL_GEOMETRY_TYPE_KAPPA6C,
	HKL_GEOMETRY_TYPE_ZAXIS,
	HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2,
	HKL_GEOMETRY_TYPE_SOLEIL_MARS,
	HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2,
	HKL_GEOMETRY_TYPE_PETRA3_P09_EH2,
	HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3,
	HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL,
} HklGeometryType;

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

struct _HklGeometryConfig {
	const char *name;
	HklGeometryType type;
};

struct _HklGeometry
{
	const HklGeometryConfig *config;
	HklSource source;
	HklAxis *axes;
	size_t len;
	HklHolder *holders;
	size_t holders_len;
};

struct _HklGeometryList
{
	int len;
	HklGeometryListMultiplyFunction multiply;
	struct list_head items;
	void *_shit;
};

struct _HklGeometryListItem
{
	HklGeometry *geometry;
	struct list_node node;
	void *_shit;
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

extern HklGeometry *hkl_geometry_new_copy(const HklGeometry *self);

extern void hkl_geometry_free(HklGeometry *self);

extern void hkl_geometry_init_geometry(HklGeometry *self,
				       const HklGeometry *src);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);

extern void hkl_geometry_update(HklGeometry *self);

extern int hkl_geometry_get_axis_idx_by_name(const HklGeometry *self,
					     const char *name);

extern HklParameter *hkl_geometry_get_axis_by_name(HklGeometry *self,
						   const char *name);

extern void hkl_geometry_randomize(HklGeometry *self);

extern int hkl_geometry_set_values_v(HklGeometry *self,
				     size_t len, ...);

extern int hkl_geometry_set_values_unit_v(HklGeometry *self, ...);

extern double hkl_geometry_distance(const HklGeometry *self,
				    const HklGeometry *ref);

extern double hkl_geometry_distance_orthodromic(const HklGeometry *self,
						const HklGeometry *ref);

extern int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self,
							 const HklGeometry *ref);

extern int hkl_geometry_is_valid(const HklGeometry *self);

extern void hkl_geometry_fprintf(FILE *file, const HklGeometry *self);

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void);

extern HklGeometryList *hkl_geometry_list_new_copy(const HklGeometryList *self);

extern void hkl_geometry_list_free(HklGeometryList *self);

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

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
