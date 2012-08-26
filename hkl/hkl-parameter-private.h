
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
#ifndef __HKL_PARAMETER_PRIVATE_H__
#define __HKL_PARAMETER_PRIVATE_H__

#include <hkl/hkl-parameter.h>

HKL_BEGIN_DECLS

struct _HklParameterOperations {
	double (*get_value)(const HklParameter *self);
	double (*get_value_unit)(const HklParameter *self);
	void (*set_value)(HklParameter *self, double value);
	void (*set_value_unit)(HklParameter *self, double value);
	void (*randomize)(HklParameter *self);
};

#define HKL_PARAMETER_OPERATIONS_DEFAULT				\
	.get_value=hkl_parameter_get_value_real,			\
		.get_value_unit = hkl_parameter_get_value_unit_real,	\
		.set_value = hkl_parameter_set_value_real,		\
		.set_value_unit = hkl_parameter_set_value_unit_real,	\
		.randomize = hkl_parameter_randomize_real

static inline double hkl_parameter_get_value_real(const HklParameter *self)
{
	return self->_value;
}

static inline double hkl_parameter_get_value_unit_real(const HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	return self->_value * factor;
}

static inline void hkl_parameter_set_value_real(HklParameter *self, double value)
{
	self->_value = value;
	self->changed = HKL_TRUE;
}

static inline void hkl_parameter_set_value_unit_real(HklParameter *self, double value)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	hkl_parameter_set_value_real(self, value / factor);
}

static inline void hkl_parameter_randomize_real(HklParameter *self)
{
	if (self->fit) {
		double alea = (double)rand() / (RAND_MAX + 1.);
		self->_value = self->range.min
			+ (self->range.max - self->range.min) * alea;
		self->changed = HKL_TRUE;
	}
}

static HklParameterOperations hkl_parameter_operations_defaults = {
	HKL_PARAMETER_OPERATIONS_DEFAULT,
};

HKL_END_DECLS

#endif /* __HKL_PARAMETER_PRIVATE_H__ */
