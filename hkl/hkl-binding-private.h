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
 * Copyright (C) 2012-2016 Synchrotron SOLEIL
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

G_BEGIN_DECLS

/**************/
/* HklFactory */
/**************/

HKLAPI GHashTable *hkl_factories(void);

/***************/
/* HklGeometry */
/***************/

HKLAPI const char **hkl_geometry_axis_names_get_binding(const HklGeometry *self,
							size_t *length) HKL_ARG_NONNULL(1, 2);


HKLAPI double* hkl_geometry_axis_values_get_binding(const HklGeometry *self, unsigned int *len,
						    HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2);

HKLAPI HklQuaternion *hkl_geometry_sample_rotation_get_binding(const HklGeometry *self,
							       const HklSample *sample) HKL_ARG_NONNULL(1, 2);

HKLAPI HklQuaternion *hkl_geometry_detector_rotation_get_binding(const HklGeometry *self,
								 const HklDetector *detector) HKL_ARG_NONNULL(1, 2);

/*******************/
/* HklGeometryList */
/*******************/

HKLAPI GSList* hkl_geometry_list_items(HklGeometryList *self) HKL_ARG_NONNULL(1);

/*************/
/* HklEngine */
/*************/

HKLAPI const char **hkl_engine_modes_names_get_binding(const HklEngine *self,
						       size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI const char **hkl_engine_pseudo_axis_names_get_binding(HklEngine *self,
							     size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI const char **hkl_engine_parameters_names_get_binding(const HklEngine *self,
							    size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI double *hkl_engine_parameters_values_get_binding(const HklEngine *self, guint *len,
							HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2);

HKLAPI const char **hkl_engine_axis_names_get_binding(const HklEngine *self,
						      HklEngineAxisNamesGet mode,
						      size_t *length) HKL_ARG_NONNULL(1, 3);

HKLAPI double *hkl_engine_pseudo_axis_values_get_binding(const HklEngine *self,
							 guint *len, HklUnitEnum unit_type);

/***************************/
/* HklPSeudoAxisEngineList */
/***************************/

HKLAPI GSList* hkl_engine_list_engines_get_as_gslist(HklEngineList *self);

/*************/
/* HklSample */
/*************/

HKLAPI GSList *hkl_sample_reflections_get(const HklSample *self);

HKLAPI HklSampleReflection *hkl_sample_add_reflection_binding(HklSample *self,
							      const HklGeometry *geometry,
							      const HklDetector *detector,
							      double h, double k, double l,
							      GError **error);

G_END_DECLS

#endif /* __HKL_BINDING_PRIVATE_H__ */
