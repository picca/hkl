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
#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <stdio.h>

#include <ccan/list/list.h>
#include <hkl/hkl-error.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-unit.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;
typedef struct _HklParameterOperations HklParameterOperations;
typedef struct _HklParameterList HklParameterList;
typedef struct _HklParameterListOperations HklParameterListOperations;

/****************/
/* HklParameter */
/****************/

struct _HklParameter {
	const char *name;
	HklInterval range;
	double _value;
	const HklUnit *unit;
	const HklUnit *punit;
	int fit;
	int changed;
	const HklParameterOperations *ops;
	struct list_node list;
	void *_shit;
};

#define HKL_PARAMETER_DEFAULTS .name="dummy", .range={.min=0, .max=0}, ._value=0, .unit=NULL, .punit=NULL, .fit=HKL_TRUE, .changed=HKL_TRUE, .ops = &hkl_parameter_operations_defaults

#define HKL_PARAMETER_DEFAULTS_ANGLE HKL_PARAMETER_DEFAULTS, .range={.min=-M_PI, .max=M_PI}, .unit = &hkl_unit_angle_rad, .punit = &hkl_unit_angle_deg

extern HklParameter *hkl_parameter_new(const char *name,
				       double min, double value, double max,
				       int fit, int changed,
				       const HklUnit *unit,
				       const HklUnit *punit);

extern HklParameter *hkl_parameter_new_copy(const HklParameter *self);

extern void hkl_parameter_free(HklParameter *self);

extern double hkl_parameter_get_value(const HklParameter *self);

extern double hkl_parameter_get_value_unit(const HklParameter *self);

extern double hkl_parameter_get_value_closest(const HklParameter *self,
					      const HklParameter *ref);

extern bool hkl_parameter_set_value(HklParameter *self, double value,
				    HklError **error);

extern bool hkl_parameter_set_value_unit(HklParameter *self, double value,
					 HklError **error);

extern double hkl_parameter_get_max(const HklParameter *self);

extern void hkl_parameter_get_range_unit(const HklParameter *self, double *min, double *max);

extern void hkl_parameter_set_range(HklParameter *self, double min, double max);

extern void hkl_parameter_set_range_unit(HklParameter *self, double min, double max);

extern void hkl_parameter_randomize(HklParameter *self);

extern int hkl_parameter_is_valid(const HklParameter *self);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

/********************/
/* HklParameterList */
/********************/

struct _HklParameterList {
	unsigned int len;
	const HklParameterListOperations *ops;
	struct list_head parameters;
	void *_shit;
};

extern void hkl_parameter_list_get_values(const HklParameterList *self,
					  double values[], unsigned int *len);

extern unsigned int hkl_parameter_list_set_values(HklParameterList *self,
						  double values[], unsigned int len,
						  HklError **error);

extern double *hkl_parameter_list_get_values_unit(const HklParameterList *self,
						  unsigned int *len);

extern unsigned int hkl_parameter_list_set_values_unit(HklParameterList *self,
						       double values[],
						       unsigned int len,
						       HklError **error);

extern HklParameter *hkl_parameter_list_get_by_name(HklParameterList *self,
						    const char *name);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
