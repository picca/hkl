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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/ccan/darray/darray.h>

#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklGeometry HklGeometry;
typedef struct _HklGeometryList HklGeometryList;
typedef struct _HklGeometryListItem HklGeometryListItem;

typedef darray(HklAxis *) darray_axis;
typedef darray(HklGeometryListItem *) darray_item;

/***************/
/* HklGeometry */
/***************/

HKLAPI void hkl_geometry_free(HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_set(HklGeometry *self, const HklGeometry *src) HKL_ARG_NONNULL(1, 2);

HKLAPI const darray_axis *hkl_geometry_axes_get(const HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_axis_set(HklGeometry *self, const HklAxis *axis) HKL_ARG_NONNULL(1, 2);

HKLAPI const char *hkl_geometry_name_get(const HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI double hkl_geometry_wavelength_get(const HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_wavelength_set(HklGeometry *self, double wavelength) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_randomize(HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_geometry_set_values_unit_v(HklGeometry *self, ...) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_fprintf(FILE *file, const HklGeometry *self) HKL_ARG_NONNULL(1, 2);

/*******************/
/* HklGeometryList */
/*******************/

HKLAPI const darray_item *hkl_geometry_list_items_get(const HklGeometryList *self) HKL_ARG_NONNULL(1);

/***********************/
/* HklGeometryListItem */
/***********************/

HKLAPI const HklGeometry *hkl_geometry_list_item_geometry_get(const HklGeometryListItem *self) HKL_ARG_NONNULL(1);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
