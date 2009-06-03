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
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

/***********/
/* HklAxis */
/***********/
static void hkl_axis_update(HklAxis *self)
{
	hkl_quaternion_from_angle_and_axe(&self->q, ((HklParameter *)self)->value, &self->axis_v);
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
	double new_distance = *distance;

	while(new_angle >= min && new_angle <= max) {
		new_distance = fabs(new_angle - current);
		if (new_distance <= *distance) {
			*angle = new_angle;
			*distance = new_distance;
		}
		new_angle += delta_angle;
	}
}


/*
 * check if the angle or its equivalent is in between [min, max]
 */
int hkl_axis_is_value_compatible_with_range(HklAxis const *self)
{
	double value;
	int res = HKL_FALSE;
	HklInterval range;

	value = ((HklParameter *)self)->value;
	range = ((HklParameter *)self)->range;

	if(hkl_interval_length(&range) > 2*M_PI)
		res = HKL_TRUE;
	else{
		hkl_interval_angle_restrict_symm(&range);
		value = gsl_sf_angle_restrict_symm(value);

		if(range.min <= range.max){
			if(range.min <= value && range.max >= value)
				res = HKL_TRUE;
		}else{
			if(value <= range.max || value >= range.min)
				res = HKL_TRUE;
		}
	}
	return res;
}

HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v)
{
	HklAxis *self = NULL;

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklAxis");

	hkl_axis_init(self, name, axis_v);

	return self;
}

void hkl_axis_free(HklAxis *self)
{
	if(self)
		free(self);
}

void hkl_axis_init(HklAxis *self, char const * name, HklVector const *axis_v)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};

	// base initializer
	hkl_parameter_init((HklParameter *)self, name, -M_PI, 0, M_PI,
			   HKL_FALSE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	self->axis_v = *axis_v;
	self->q = q0;
}

char const *hkl_axis_get_name(HklAxis const *self)
{
	return ((HklParameter *)self)->name;
}

int hkl_axis_get_changed(HklAxis const *self)
{
	return ((HklParameter *)self)->changed;
}

void hkl_axis_set_changed(HklAxis *self, int changed)
{
	((HklParameter *)self)->changed = changed;
}

double hkl_axis_get_value(HklAxis const *self)
{
	return ((HklParameter *)self)->value;
}

double hkl_axis_get_value_unit(HklAxis const *self)
{
	return hkl_parameter_get_value_unit((HklParameter *)self);
}

double hkl_axis_get_value_closest(HklAxis const *self, HklAxis const *axis)
{
	double angle = ((HklParameter *)self)->value;

	if(hkl_axis_is_value_compatible_with_range(self)){
		if(hkl_interval_length(&((HklParameter *)self)->range) >= 2*M_PI){
			int k;
			double current = ((HklParameter *)axis)->value;
			double distance = fabs(current - angle);
			double delta = 2. * M_PI;
			double min = ((HklParameter *)self)->range.min;
			double max = ((HklParameter *)self)->range.max;

			// three cases
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

double hkl_axis_get_value_closest_unit(HklAxis const *self, HklAxis const *axis)
{
	double factor = hkl_unit_factor(((HklParameter *)self)->unit, ((HklParameter *)self)->punit);
	return factor * hkl_axis_get_value_closest(self, axis);
}

double hkl_axis_get_max(HklAxis const *self)
{
	return hkl_parameter_get_max((HklParameter *)self);
}

void hkl_axis_get_range_unit(HklAxis const *self, double *min, double *max)
{
	hkl_parameter_get_range_unit((HklParameter *)self, min, max);
}

void hkl_axis_set_value(HklAxis *self, double value)
{
	hkl_parameter_set_value((HklParameter *)self, value);
	hkl_axis_update(self);
}

void hkl_axis_set_value_smallest_in_range(HklAxis *self)
{
	double value, min;

	value = ((HklParameter *)self)->value;
	min = ((HklParameter *)self)->range.min;

	if(value < min)
		hkl_axis_set_value(self, value + 2*M_PI*ceil((min - value)/(2*M_PI)));
	else
		hkl_axis_set_value(self, value - 2*M_PI*floor((value - min)/(2*M_PI)));
}

void hkl_axis_set_value_unit(HklAxis *self, double value)
{
	hkl_parameter_set_value_unit((HklParameter *)self, value);
	hkl_axis_update(self);
}

void hkl_axis_set_range(HklAxis *self, double min, double max)
{
	hkl_parameter_set_range((HklParameter *)self, min, max);
}

void hkl_axis_set_range_unit(HklAxis *self, double min, double max)
{
	hkl_parameter_set_range_unit((HklParameter *)self, min, max);
}

void hkl_axis_randomize(HklAxis *self)
{
	hkl_parameter_randomize((HklParameter *)self);
	hkl_axis_update(self);
}

void hkl_axis_get_quaternion(HklAxis const *self, HklQuaternion *q)
{
	hkl_quaternion_from_angle_and_axe(q, ((HklParameter *)self)->value,
					  &self->axis_v);
}

void hkl_axis_fprintf(FILE *f, HklAxis *self)
{
	hkl_parameter_fprintf(f, (HklParameter *)self);
	hkl_vector_fprintf(f, &self->axis_v);
	hkl_quaternion_fprintf(f, &self->q);
}
