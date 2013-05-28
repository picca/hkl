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
#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <stdbool.h>

#include <hkl/ccan/darray/darray.h>
#include <hkl/hkl-macros.h>
#include <hkl/hkl-error.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;
typedef struct _HklParameterList HklParameterList;
typedef HklParameterList darray_parameter;

/****************/
/* HklParameter */
/****************/

HKLAPI const char *hkl_parameter_name_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI double hkl_parameter_value_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI bool hkl_parameter_value_set(HklParameter *self, double value,
				    HklError **error) HKL_ARG_NONNULL(1);

HKLAPI double hkl_parameter_value_unit_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI bool hkl_parameter_value_unit_set(HklParameter *self, double value,
					 HklError **error) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_min_max_get(const HklParameter *self, double *min, double *max) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI void hkl_parameter_min_max_set(HklParameter *self, double min, double max) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_min_max_unit_get(const HklParameter *self, double *min, double *max) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI void hkl_parameter_min_max_unit_set(HklParameter *self, double min, double max) HKL_ARG_NONNULL(1);

HKLAPI bool hkl_parameter_fit_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_fit_set(HklParameter *self, bool fit) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_randomize(HklParameter *self) HKL_ARG_NONNULL(1);

/********************/
/* HklParameterList */
/********************/

struct _HklParameterList {
	_darray(HklParameter *);
};

HKLAPI unsigned int hkl_parameter_list_values_set(HklParameterList *self,
						  double values[], unsigned int len,
						  HklError **error) HKL_ARG_NONNULL(1);

HKLAPI double *hkl_parameter_list_values_unit_get(const HklParameterList *self,
						  unsigned int *len) HKL_ARG_NONNULL(1, 2);

/* only use in the test method for now */
HKLAPI void hkl_parameter_list_randomize(HklParameterList *self) HKL_ARG_NONNULL(1);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
