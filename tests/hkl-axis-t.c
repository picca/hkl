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
#include <hkl.h>
#include <tap/basic.h>
#include <tap/float.h>

#include <hkl/ccan/container_of/container_of.h>
#include "hkl-axis-private.h"

static void new(void)
{
	HklParameter *axis;
	static HklVector v = {{1, 0, 0}};

	axis = hkl_parameter_new_axis("omega", &v);

	is_string("omega", axis->name, __func__);
	is_double(-M_PI, axis->range.min, HKL_EPSILON, __func__);
	is_double(M_PI, axis->range.max, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(axis), HKL_EPSILON, __func__);
	ok(HKL_TRUE == axis->fit, __func__);
	ok(HKL_TRUE == axis->changed, __func__);

	hkl_parameter_free(axis);
}

static void get_quaternions(void)
{
	HklAxis *axis;
	static HklVector v = {{1, 0, 0}};

	axis = container_of(hkl_parameter_new_axis("omega", &v),
			    HklAxis, parameter);

	is_double(1., axis->q.data[0], HKL_EPSILON, __func__);
	is_double(0., axis->q.data[1], HKL_EPSILON, __func__);
	is_double(0., axis->q.data[2], HKL_EPSILON, __func__);
	is_double(0., axis->q.data[3], HKL_EPSILON, __func__);

	hkl_parameter_value_set(&axis->parameter, -M_PI_2, NULL);
	is_double(1./sqrt(2.), axis->q.data[0], HKL_EPSILON, __func__);
	is_double(-1./sqrt(2.), axis->q.data[1], HKL_EPSILON, __func__);
	is_double(0., axis->q.data[2], HKL_EPSILON, __func__);
	is_double(0., axis->q.data[3], HKL_EPSILON, __func__);

	hkl_parameter_free(&axis->parameter);
}

static void copy(void)
{
	HklAxis *axis;
	HklAxis *copy;
	static HklVector v = {{1, 0, 0}};

	axis = container_of(hkl_parameter_new_axis("omega", &v),
			    HklAxis, parameter);
	hkl_parameter_value_set(&axis->parameter, -M_PI_2, NULL);

	copy = container_of(hkl_parameter_new_copy(&axis->parameter),
			    HklAxis, parameter);
	is_string("omega", copy->parameter.name, __func__);
	is_double(-M_PI, copy->parameter.range.min, HKL_EPSILON, __func__);
	is_double(M_PI, copy->parameter.range.max, HKL_EPSILON, __func__);
	is_double(-M_PI_2, hkl_parameter_value_get(&copy->parameter), HKL_EPSILON, __func__);
	ok(HKL_TRUE == copy->parameter.fit, __func__);
	ok(HKL_TRUE == copy->parameter.changed, __func__);
	is_double(1./sqrt(2.), copy->q.data[0], HKL_EPSILON, __func__);
	is_double(-1./sqrt(2.), copy->q.data[1], HKL_EPSILON, __func__);
	is_double(0., copy->q.data[2], HKL_EPSILON, __func__);
	is_double(0., copy->q.data[3], HKL_EPSILON, __func__);
	
	hkl_parameter_free(&axis->parameter);
	hkl_parameter_free(&copy->parameter);
}

static void is_valid(void)
{
	HklAxis *axis1;
	static HklVector v = {{1, 0, 0}};

	axis1 = container_of(hkl_parameter_new_axis("omega", &v),
			     HklAxis, parameter);

	hkl_parameter_value_unit_set(&axis1->parameter, 45, NULL);
	ok(HKL_TRUE == hkl_parameter_is_valid(&axis1->parameter), __func__);

	/* change the range of axis1 */
	hkl_parameter_min_max_unit_set(&axis1->parameter, -270, 0);
	ok(HKL_FALSE == hkl_parameter_is_valid(&axis1->parameter), __func__);

	hkl_parameter_value_unit_set(&axis1->parameter, -45, NULL);
	ok(HKL_TRUE == hkl_parameter_is_valid(&axis1->parameter), __func__);

	hkl_parameter_min_max_unit_set(&axis1->parameter, 350, 450);
	hkl_parameter_value_unit_set(&axis1->parameter, 45, NULL);
	ok(HKL_TRUE == hkl_parameter_is_valid(&axis1->parameter), __func__);
	hkl_parameter_value_unit_set(&axis1->parameter, -45, NULL);
	ok(HKL_FALSE == hkl_parameter_is_valid(&axis1->parameter), __func__);

	hkl_parameter_min_max_unit_set(&axis1->parameter, -10, 90);
	hkl_parameter_value_unit_set(&axis1->parameter, 405, NULL);
	ok(HKL_TRUE == hkl_parameter_is_valid(&axis1->parameter), __func__);
	hkl_parameter_value_unit_set(&axis1->parameter, -405, NULL);
	ok(HKL_FALSE == hkl_parameter_is_valid(&axis1->parameter), __func__);

	hkl_parameter_free(&axis1->parameter);
}

static void set_value_smallest_in_range(void)
{
	HklAxis *axis;
	static HklVector v = {{1, 0, 0}};

	axis = container_of(hkl_parameter_new_axis("omega", &v),
			    HklAxis, parameter);

	hkl_parameter_min_max_unit_set(&axis->parameter, -190, 190);

	hkl_parameter_value_unit_set(&axis->parameter, 185, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-175., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis->parameter, 545, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-175., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis->parameter, -185, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-185., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis->parameter, 175, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-185., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis->parameter, 190, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-170., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis->parameter, -190, NULL);
	hkl_parameter_value_set_smallest_in_range(&axis->parameter);
	is_double(-190., hkl_parameter_value_unit_get(&axis->parameter), HKL_EPSILON, __func__);

	hkl_parameter_free(&axis->parameter);
}

static void get_value_closest(void)
{
	HklAxis *axis1, *axis2;
	static HklVector v = {{1, 0, 0}};

	axis1 = container_of(hkl_parameter_new_axis("omega", &v),
			     HklAxis, parameter);
	axis2 = container_of(hkl_parameter_new_axis("omega", &v),
			     HklAxis, parameter);

	hkl_parameter_value_unit_set(&axis1->parameter, 0, NULL);
	hkl_parameter_value_unit_set(&axis2->parameter, 0, NULL);
	is_double(0., hkl_parameter_value_get_closest(&axis1->parameter,
						      &axis2->parameter),
		  HKL_EPSILON, __func__);

	/* change the range of axis1 */
	hkl_parameter_min_max_unit_set(&axis1->parameter, -270, 180);
	hkl_parameter_value_unit_set(&axis1->parameter, 100, NULL);

	hkl_parameter_value_unit_set(&axis2->parameter, -75, NULL);
	is_double(100 * HKL_DEGTORAD, hkl_parameter_value_get_closest(&axis1->parameter,
								      &axis2->parameter),
		  HKL_EPSILON, __func__);

	hkl_parameter_value_unit_set(&axis2->parameter, -85, NULL);
	is_double(-260 * HKL_DEGTORAD, hkl_parameter_value_get_closest(&axis1->parameter,
								       &axis2->parameter),
		  HKL_EPSILON, __func__);

	hkl_parameter_free(&axis1->parameter);
	hkl_parameter_free(&axis2->parameter);
}

int main(int argc, char** argv)
{
	plan(40);

	new();
	get_quaternions();
	copy();
	is_valid();
	set_value_smallest_in_range();
	get_value_closest();

	return 0;
}
