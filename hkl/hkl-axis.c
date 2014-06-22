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
#include <gsl/gsl_nan.h>                // for GSL_NAN
#include <gsl/gsl_sf_trig.h>            // for gsl_sf_angle_restrict_symm
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include <math.h>                       // for M_PI, ceil, fabs, floor
#include <stdio.h>                      // for FILE
#include <stdlib.h>                     // for NULL, free
#include "hkl-axis-private.h"           // for HklAxis
#include "hkl-interval-private.h"       // for HklInterval, etc
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-quaternion-private.h"     // for hkl_quaternion_fprintf, etc
#include "hkl-vector-private.h"         // for hkl_vector_fprintf, etc
#include "hkl.h"                        // for HklParameter, TRUE, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of

/***********/
/* HklAxis */
/***********/

static inline HklParameter *hkl_axis_copy_real(const HklParameter *base)
{
	HklAxis *self = container_of(base, HklAxis, parameter);
	HklAxis *dup;

	dup = HKL_MALLOC(HklAxis);

	*dup = *self;

	return &dup->parameter;
}


static inline void hkl_axis_free_real(HklParameter *self)
{
	free(container_of(self, HklAxis, parameter));
}

static inline void hkl_axis_update(HklAxis *self)
{
	hkl_quaternion_init_from_angle_and_axe(&self->q,
					       self->parameter._value,
					       &self->axis_v);
}

static inline int hkl_axis_init_copy_real(HklParameter *self, const HklParameter *src,
					  GError **error)
{
	HklAxis *axis_self = container_of(self, HklAxis, parameter);
	HklAxis *axis_src = container_of(src, HklAxis, parameter);

	g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

	*axis_self = *axis_src;
	self->changed = TRUE;

	return TRUE;
}

static inline int hkl_axis_set_value_real(HklParameter *self, double value,
					  GError **error)
{
	HklAxis *axis = container_of(self, HklAxis, parameter);

	g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

	if(!hkl_parameter_value_set_real(self, value, error)){
		g_assert (error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_axis_update(axis);

	return TRUE;
}

static inline int hkl_axis_set_value_unit_real(HklParameter *self, double value,
					       GError **error)
{
	HklAxis *axis = container_of(self, HklAxis, parameter);

	g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

	if(!hkl_parameter_value_unit_set_real(self, value, error)){
		g_assert (error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_axis_update(axis);

	return TRUE;
}

static inline void hkl_axis_set_value_smallest_in_range_real(HklParameter *self)
{
	double value, min;

	value = self->_value;
	min = self->range.min;

	if(value < min)
		hkl_axis_set_value_real(self,
					value + 2*M_PI*ceil((min - value)/(2*M_PI)),
					NULL);
	else
		hkl_axis_set_value_real(self,
					value - 2*M_PI*floor((value - min)/(2*M_PI)),
					NULL);
}

static inline void hkl_axis_randomize_real(HklParameter *self)
{
	hkl_parameter_randomize_real(self);
	hkl_axis_update(container_of(self, HklAxis, parameter));
}

/*
 * given a current position of angle a min and max interval find the closest
 * equivalent angle + n delta_angle in a given direction.
 * CAUSION angle MUST be in [min, max] otherwise...
 */
static void find_angle(double current, double *angle, double *distance,
		       double min, double max, double delta_angle)
{
	double new_angle = *angle;
	double new_distance;

	while(new_angle >= min && new_angle <= max) {
		new_distance = fabs(new_angle - current);
		if (new_distance <= *distance) {
			*angle = new_angle;
			*distance = new_distance;
		}
		new_angle += delta_angle;
	}
}

static inline double hkl_axis_get_value_closest_real(const HklParameter *self,
						     const HklParameter *ref)
{
	double angle = self->_value;

	if(hkl_parameter_is_valid(self)){
		if(hkl_interval_length(&self->range) >= 2*M_PI){
			int k;
			double current = ref->_value;
			double distance = fabs(current - angle);
			double delta = 2. * M_PI;
			double min = self->range.min;
			double max = self->range.max;

			/* three cases */
			if (angle > max) {
				k = (int)(floor((max - angle) / delta));
				angle += k * delta;
				find_angle(current, &angle, &distance, min, max, -delta);
			} else if (angle < min) {
				k = (int) (ceil((min - angle) / delta));
				angle += k * delta;
				find_angle(current, &angle, &distance, min, max, delta);
			} else {
				find_angle(current, &angle, &distance, min, max, -delta);
				find_angle(current, &angle, &distance, min, max, delta);
			}
		}

	}else
		angle = GSL_NAN;
	return angle;
}

/*
 * check if the angle or its equivalent is in between [min, max]
 */
static inline int hkl_axis_is_valid_real(const HklParameter *self)
{
	double value = self->_value;
	int res = FALSE;
	HklInterval range = self->range;

	if(hkl_interval_length(&range) > 2*M_PI)
		res = TRUE;
	else{
		hkl_interval_angle_restrict_symm(&range);
		value = gsl_sf_angle_restrict_symm(value);

		if(range.min <= range.max){
			if(range.min <= value && range.max >= value)
				res = TRUE;
		}else{
			if(value <= range.max || value >= range.min)
				res = TRUE;
		}
	}
	return res;
}

static inline void hkl_axis_fprintf_real(FILE *f, const HklParameter *self)
{
	HklAxis *axis = container_of(self, HklAxis, parameter);

	hkl_parameter_fprintf_real(f, self);
	hkl_vector_fprintf(f, &axis->axis_v);
	hkl_quaternion_fprintf(f, &axis->q);
}

static HklParameterOperations hkl_parameter_operations_axis = {
	HKL_PARAMETER_OPERATIONS_DEFAULTS,
	.copy = hkl_axis_copy_real,
	.free = hkl_axis_free_real,
	.init_copy = hkl_axis_init_copy_real,
	.get_value_closest = hkl_axis_get_value_closest_real,
	.set_value = hkl_axis_set_value_real,
	.set_value_unit = hkl_axis_set_value_unit_real,
	.set_value_smallest_in_range = hkl_axis_set_value_smallest_in_range_real,
	.randomize = hkl_axis_randomize_real,
	.is_valid = hkl_axis_is_valid_real,
	.fprintf = hkl_axis_fprintf_real
};

HklParameter *hkl_parameter_new_axis(char const *name, HklVector const *axis_v)
{
	HklAxis *self = NULL;
	static HklAxis axis0 = {
		.parameter = {
			HKL_PARAMETER_DEFAULTS_ANGLE,
			.ops = &hkl_parameter_operations_axis,
		},
		.q = {{1, 0, 0, 0}},
	};

	self = HKL_MALLOC(HklAxis);


	*self = axis0;
	self->parameter.name = name;
	self->axis_v = *axis_v;

	return &self->parameter;
}
