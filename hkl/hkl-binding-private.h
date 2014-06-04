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
 * Copyright (C) 2012-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_BINDING_PRIVATE_H__
#define __HKL_BINDING_PRIVATE_H__

#include <glib.h>                       // for GError, GHashTable, etc
#include <sys/types.h>                  // for uint
#include "hkl.h"                        // for HKLAPI, HklGeometry, etc

HKL_BEGIN_DECLS

/**************/
/* HklFactory */
/**************/

HKLAPI GHashTable *hkl_factories(void);

/***************/
/* HklGeometry */
/***************/

HKLAPI GSList* hkl_geometry_axes(HklGeometry *self);

HKLAPI double* hkl_geometry_get_axes_values_unit(const HklGeometry *self, unsigned int *len);

HKLAPI void hkl_geometry_set_axes_values_unit(HklGeometry *self, double *values, unsigned int len);

/*******************/
/* HklGeometryList */
/*******************/

HKLAPI GSList* hkl_geometry_list_items(HklGeometryList *self);

/***********************/
/* HklGeometryListItem */
/***********************/

HKLAPI const HklGeometry *hkl_geometry_list_item_geometry(const HklGeometryListItem *self);

/*************/
/* HklEngine */
/*************/

HKLAPI const char **hkl_engine_modes_get_binding(const HklEngine *self,
						 size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI const char **hkl_engine_parameters_get_binding(const HklEngine *self,
						      size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI double *hkl_engine_pseudo_axes_values_get_binding(const HklEngine *self,
							 guint *len);

HKLAPI gboolean hkl_engine_set_values_unit(HklEngine *self,
					   double values[], unsigned int len,
					   GError **error);

/***************************/
/* HklPSeudoAxisEngineList */
/***************************/

HKLAPI GSList* hkl_engine_list_engines_get_as_gslist(HklEngineList *self);

/*************/
/* HklSample */
/*************/

HKLAPI const GSList *hkl_sample_reflections_get(const HklSample *self);

HKLAPI HklSampleReflection *hkl_sample_add_reflection_binding(HklSample *self,
							      const HklGeometry *geometry,
							      const HklDetector *detector,
							      double h, double k, double l);

HKL_END_DECLS

#endif /* __HKL_BINDING_PRIVATE_H__ */
