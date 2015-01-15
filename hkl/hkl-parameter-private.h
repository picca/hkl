
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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PARAMETER_PRIVATE_H__
#define __HKL_PARAMETER_PRIVATE_H__

#include <math.h>                       // for M_PI
#include <stdio.h>                      // for FILE, fprintf, NULL
#include <stdlib.h>                     // for free, rand, RAND_MAX
#include "hkl-interval-private.h"       // for HklInterval
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-unit-private.h"           // for HklUnit, hkl_unit_factor
#include "hkl.h"                        // for HklParameter, TRUE, etc

G_BEGIN_DECLS

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

#define HKL_PARAMETER_DEFAULTS .name="dummy", .range={.min=0, .max=0}, ._value=0, .unit=NULL, .punit=NULL, .fit=TRUE, .changed=TRUE, .ops = &hkl_parameter_operations_defaults

#define HKL_PARAMETER_DEFAULTS_ANGLE HKL_PARAMETER_DEFAULTS, .range={.min=-M_PI, .max=M_PI}, .unit = &hkl_unit_angle_rad, .punit = &hkl_unit_angle_deg

#define HKL_PARAMETER_ERROR hkl_parameter_error_quark ()

static GQuark hkl_parameter_error_quark (void)
{
	return g_quark_from_static_string ("hkl-parameter-error-quark");
}

typedef enum {
	HKL_PARAMETER_ERROR_MIN_MAX_SET, /* can not set the min max */
} HklParameterError;

/****************/
/* HklParameter */
/****************/

struct _HklParameterOperations {
	HklParameter *        (*copy)(const HklParameter *self);
	void                  (*free)(HklParameter *self);
	int                   (*init_copy)(HklParameter *self, const HklParameter *src,
					   GError **error);
	double                (*get_value_closest)(const HklParameter *self,
						   const HklParameter *other);
	int                   (*set_value)(HklParameter *self, double value,
					   HklUnitEnum unit_type, GError **error);
	void                  (*set_value_smallest_in_range)(HklParameter *self);
	void                  (*randomize)(HklParameter *self);
	int                   (*is_valid)(const HklParameter *self);
	void                  (*fprintf)(FILE *f, const HklParameter *self);
	const HklVector *     (*axis_v_get)(const HklParameter *self);
	const HklQuaternion * (*quaternion_get)(const HklParameter *self);
};

#define HKL_PARAMETER_OPERATIONS_DEFAULTS				\
	.copy = hkl_parameter_copy_real,				\
		.free = hkl_parameter_free_real,			\
		.init_copy = hkl_parameter_init_copy_real,		\
		.get_value_closest = hkl_parameter_value_get_closest_real, \
		.set_value = hkl_parameter_value_set_real,		\
		.set_value_smallest_in_range = hkl_parameter_value_set_smallest_in_range_real, \
		.randomize = hkl_parameter_randomize_real,		\
		.is_valid = hkl_parameter_is_valid_real,		\
		.fprintf = hkl_parameter_fprintf_real,			\
		.axis_v_get = hkl_parameter_axis_v_get_real,		\
		.quaternion_get = hkl_parameter_quaternion_get_real

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

static inline int hkl_parameter_init_copy_real(HklParameter *self, const HklParameter *src,
					       GError **error)
{
	hkl_error (error == NULL || *error == NULL);
	hkl_error (self->name == src->name || strcmp(self->name, src->name) == 0);

	*self = *src;
	self->changed = TRUE;

	return TRUE;
}

static inline double hkl_parameter_value_get_closest_real(const HklParameter *self,
							  const HklParameter *ref)
{
	return self->_value;
}

static inline int hkl_parameter_value_set_real(HklParameter *self, double value,
					       HklUnitEnum unit_type, GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	switch (unit_type) {
	case HKL_UNIT_DEFAULT:
		self->_value = value;
		break;
	case HKL_UNIT_USER:
		self->_value = value / hkl_unit_factor(self->unit, self->punit);
		break;
	}
	self->changed = TRUE;

	return TRUE;
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
		self->changed = TRUE;
	}
}

static inline int hkl_parameter_is_valid_real(const HklParameter *self)
{
	if(self->_value < (self->range.min - HKL_EPSILON)
	   || self->_value > (self->range.max + HKL_EPSILON))
		return FALSE;
	else
		return TRUE;
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

static inline const HklVector *hkl_parameter_axis_v_get_real(const HklParameter *self)
{
	return NULL;
}

static inline const HklQuaternion *hkl_parameter_quaternion_get_real(const HklParameter *self)
{
	return NULL;
}

static HklParameterOperations hkl_parameter_operations_defaults = {
	HKL_PARAMETER_OPERATIONS_DEFAULTS,
};


extern HklParameter *hkl_parameter_new(const char *name,
				       double min, double value, double max,
				       int fit, int changed,
				       const HklUnit *unit,
				       const HklUnit *punit);

extern int hkl_parameter_init_copy(HklParameter *self, const HklParameter *src,
				   GError **error);

extern double hkl_parameter_value_get_closest(const HklParameter *self,
					      const HklParameter *ref);

extern void hkl_parameter_value_set_smallest_in_range(HklParameter *self);

extern int hkl_parameter_is_valid(const HklParameter *self);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

/********************/
/* HklParameterList */
/********************/

typedef darray(HklParameter *) darray_parameter;

G_END_DECLS

#endif /* __HKL_PARAMETER_PRIVATE_H__ */
