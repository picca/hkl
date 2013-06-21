
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
#ifndef __HKL_PARAMETER_PRIVATE_H__
#define __HKL_PARAMETER_PRIVATE_H__

#include <stdio.h>

#include "hkl-parameter.h"
#include "hkl-interval-private.h"
#include "hkl-unit-private.h"

HKL_BEGIN_DECLS

typedef struct _HklParameterOperations HklParameterOperations;

struct _HklParameter {
	const char *name;
	HklInterval range;
	double _value;
	const HklUnit *unit;
	const HklUnit *punit;
	int fit;
	int changed;
	const HklParameterOperations *ops;
};

#define HKL_PARAMETER_DEFAULTS .name="dummy", .range={.min=0, .max=0}, ._value=0, .unit=NULL, .punit=NULL, .fit=HKL_TRUE, .changed=HKL_TRUE, .ops = &hkl_parameter_operations_defaults

#define HKL_PARAMETER_DEFAULTS_ANGLE HKL_PARAMETER_DEFAULTS, .range={.min=-M_PI, .max=M_PI}, .unit = &hkl_unit_angle_rad, .punit = &hkl_unit_angle_deg


/****************/
/* HklParameter */
/****************/

struct _HklParameterOperations {
	HklParameter * (*copy)(const HklParameter *self);
	void           (*free)(HklParameter *self);
	void           (*init_copy)(HklParameter *self, const HklParameter *src);
	double         (*get_value_closest)(const HklParameter *self,
				    const HklParameter *other);
	unsigned int   (*set_value)(HklParameter *self, double value,
				  HklError **error);
	unsigned int   (*set_value_unit)(HklParameter *self, double value,
				       HklError **error);
	void           (*set_value_smallest_in_range)(HklParameter *self);
	void           (*randomize)(HklParameter *self);
	int            (*is_valid)(const HklParameter *self);
	void           (*fprintf)(FILE *f, const HklParameter *self);
};

#define HKL_PARAMETER_OPERATIONS_DEFAULTS				\
	.copy = hkl_parameter_copy_real,				\
		.free = hkl_parameter_free_real,			\
		.init_copy = hkl_parameter_init_copy_real,		\
		.get_value_closest = hkl_parameter_value_get_closest_real, \
		.set_value = hkl_parameter_value_set_real,		\
		.set_value_unit = hkl_parameter_value_unit_set_real,	\
		.set_value_smallest_in_range = hkl_parameter_value_set_smallest_in_range_real, \
		.randomize = hkl_parameter_randomize_real,		\
		.is_valid = hkl_parameter_is_valid_real,		\
		.fprintf = hkl_parameter_fprintf_real

static inline HklParameter *hkl_parameter_copy_real(const HklParameter *self)
{
	HklParameter *dup = HKL_MALLOC(HklParameter);

	*dup = *self;

	return dup;
}

static inline void hkl_parameter_free_real(HklParameter *self)
{
	free(self);
}

static inline void hkl_parameter_init_copy_real(HklParameter *self, const HklParameter *src)
{
	*self = *src;
	self->changed = HKL_TRUE;
}

static inline double hkl_parameter_value_get_closest_real(const HklParameter *self,
							  const HklParameter *ref)
{
	return self->_value;
}

static inline unsigned int hkl_parameter_value_set_real(
	HklParameter *self, double value,
	HklError **error)
{
	self->_value = value;
	self->changed = HKL_TRUE;

	return HKL_TRUE;
}

static inline unsigned int hkl_parameter_value_unit_set_real(
	HklParameter *self, double value,
	HklError **error)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	return hkl_parameter_value_set_real(self, value / factor, error);
}

static inline void hkl_parameter_value_set_smallest_in_range_real(HklParameter *self)
{
	/* DOES NOTHING for a standard parameter */
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

static inline int hkl_parameter_is_valid_real(const HklParameter *self)
{
	if(self->_value < (self->range.min - HKL_EPSILON)
	   || self->_value > (self->range.max + HKL_EPSILON))
		return HKL_FALSE;
	else
		return HKL_TRUE;
}

static inline void hkl_parameter_fprintf_real(FILE *f, const HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	if (self->punit)
		fprintf(f, "\"%s\" : %.7f %s [%.7f : %.7f] (%d)",
			self->name,
			self->_value * factor,
			self->punit->repr,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
	else
		fprintf(f, "\"%s\" : %.7f [%.7f : %.7f] (%d)",
			self->name,
			self->_value * factor,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
}

static HklParameterOperations hkl_parameter_operations_defaults = {
	HKL_PARAMETER_OPERATIONS_DEFAULTS,
};


extern HklParameter *hkl_parameter_new(const char *name,
				       double min, double value, double max,
				       int fit, int changed,
				       const HklUnit *unit,
				       const HklUnit *punit);

extern HklParameter *hkl_parameter_new_copy(const HklParameter *self);

extern void hkl_parameter_free(HklParameter *self);

extern void hkl_parameter_init_copy(HklParameter *self, const HklParameter *src);

extern double hkl_parameter_value_get_closest(const HklParameter *self,
					      const HklParameter *ref);

extern void hkl_parameter_value_set_smallest_in_range(HklParameter *self);

extern int hkl_parameter_is_valid(const HklParameter *self);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

/********************/
/* HklParameterList */
/********************/

extern void hkl_parameter_list_values_get(const HklParameterList *self,
					  double values[], unsigned int *len);

extern unsigned int hkl_parameter_list_values_unit_set(HklParameterList *self,
						       double values[],
						       unsigned int len,
						       HklError **error);

extern void hkl_parameter_list_free(HklParameterList *self);

extern void hkl_parameter_list_fprintf(FILE *f, const HklParameterList *self);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_PRIVATE_H__ */
