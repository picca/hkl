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
#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <stdio.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-unit.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;

struct _HklParameter {
	const char *name;
	HklInterval range;
	double value;
	HklUnit const *unit;
	HklUnit const *punit;
	int not_to_fit;
	int changed;
};

extern HklParameter *hkl_parameter_new(char const *name,
				       double min, double value, double max,
				       int not_to_fit, int changed,
				       HklUnit const *unit,
				       HklUnit const *punit);

extern HklParameter *hkl_parameter_new_copy(HklParameter const *self);

extern int hkl_parameter_init(HklParameter *self, char const *name,
			      double min, double value, double max,
			      int not_to_fit, int changed,
			      HklUnit const *unit, HklUnit const *punit);

extern void hkl_parameter_free(HklParameter *self);

//extern double hkl_parameter_get_value(HklParameter const *self);

extern void hkl_parameter_set_value(HklParameter *self, double value);

extern double hkl_parameter_get_value_unit(HklParameter const *self);

extern int hkl_parameter_set_value_unit(HklParameter *self, double value);

extern double hkl_parameter_get_max(HklParameter const *self);

extern void hkl_parameter_get_range_unit(HklParameter const *self, double *min, double *max);

extern void hkl_parameter_set_range(HklParameter *self, double min, double max);

extern void hkl_parameter_set_range_unit(HklParameter *self, double min, double max);

extern void hkl_parameter_randomize(HklParameter *self);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
