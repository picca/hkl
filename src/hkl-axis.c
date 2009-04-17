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

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

/***********/
/* HklAxis */
/***********/
static void hkl_axis_update(HklAxis *self)
{
	hkl_quaternion_from_angle_and_axe(&self->q, self->parent.value, &self->axis_v);
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
static int hkl_axis_is_value_compatible_with_range(HklAxis const *self)
{
	double c = cos(self->parent.value);
	HklInterval c_r = self->parent.range;

	hkl_interval_cos(&c_r);

	if (c_r.min <= c && c <= c_r.max) {
		double s = sin(self->parent.value);
		HklInterval s_r = self->parent.range;

		hkl_interval_sin(&s_r);

		if (s_r.min <= s && s <= s_r.max)
			return HKL_TRUE;
	}
	return HKL_FALSE; 
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

double hkl_axis_get_value_unit(HklAxis const *self)
{
	return hkl_parameter_get_value_unit(&self->parent);
}

double hkl_axis_get_value_closest(HklAxis const *self, HklAxis const *axis)
{
	double angle = self->parent.value;

	if(hkl_axis_is_value_compatible_with_range(self)){
		if(hkl_interval_length(&self->parent.range) >= 2*M_PI){
			int k;
			double current = axis->parent.value;
			double distance = fabs(current - angle);
			double delta = 2. * M_PI;
			double min = self->parent.range.min;
			double max = self->parent.range.max;

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
	double factor = hkl_unit_factor(self->parent.unit, self->parent.punit);
	return factor * hkl_axis_get_value_closest(self, axis);
}

void hkl_axis_get_range_unit(HklAxis const *self, double *min, double *max)
{
	hkl_parameter_get_range_unit(&self->parent, min, max);
}

void hkl_axis_set_value(HklAxis *self, double value)
{
	hkl_parameter_set_value(&self->parent, value);
	hkl_axis_update(self);
}

void hkl_axis_set_value_unit(HklAxis *self, double value)
{
	hkl_parameter_set_value_unit(&self->parent, value);
	hkl_axis_update(self);
}

void hkl_axis_set_range(HklAxis *self, double min, double max)
{
	hkl_parameter_set_range(&self->parent, min, max);
}

void hkl_axis_set_range_unit(HklAxis *self, double min, double max)
{
	hkl_parameter_set_range_unit(&self->parent, min, max);
}

void hkl_axis_randomize(HklAxis *self)
{
	hkl_parameter_randomize(&self->parent);
	hkl_axis_update(self);
}

void hkl_axis_get_quaternion(HklAxis const *self, HklQuaternion *q)
{
	hkl_quaternion_from_angle_and_axe(q, ((HklParameter *)self)->value,
					  &self->axis_v);
}

void hkl_axis_fprintf(FILE *f, HklAxis *self)
{
	hkl_parameter_fprintf(f, &self->parent);
	hkl_vector_fprintf(f, &self->axis_v);
	hkl_quaternion_fprintf(f, &self->q);
}
