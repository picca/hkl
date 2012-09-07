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
 * Copyright (C) 2012      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_BINDING_PRIVATE_H__
#define __HKL_BINDING_PRIVATE_H__

#include <glib/gslist.h>
#include <glib/gerror.h>

#include "hkl-pseudoaxis.h"

HKL_BEGIN_DECLS

/********************/
/* HklParameterList */
/********************/

extern GSList* hkl_parameter_list_parameters(HklParameterList *self);

extern gboolean hkl_parameter_list_set_values_unit_binding(HklParameterList *self,
							   double *values, uint len,
							   GError **error);
/***************/
/* HklGeometry */
/***************/

extern GSList* hkl_geometry_axes(HklGeometry *self);

extern double* hkl_geometry_get_axes_values_unit(const HklGeometry *self, unsigned int *len);

extern void hkl_geometry_set_axes_values_unit(HklGeometry *self, double *values, unsigned int len);

/*******************/
/* HklGeometryList */
/*******************/

extern GSList* hkl_geometry_list_items(HklGeometryList *self);

/***********************/
/* HklPseudoAxisEngine */
/***********************/

extern GSList* hkl_pseudo_axis_engine_modes(HklPseudoAxisEngine *self);

extern gboolean hkl_pseudo_axis_engine_set_values_unit(
	HklPseudoAxisEngine *self,
	double values[], unsigned int len,
	GError **error);

/***************************/
/* HklPSeudoAxisEngineList */
/***************************/

extern GSList* hkl_engine_list_engines(HklEngineList *self);

HKL_END_DECLS

#endif /* __HKL_BINDING_PRIVATE_H__ */
