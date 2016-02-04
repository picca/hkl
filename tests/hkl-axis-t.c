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
 * Copyright (C) 2003-2016 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

/* use these private methods */
HklParameter *hkl_parameter_new_axis(const char *, const HklVector *axis_v);
int hkl_parameter_is_valid(const HklParameter *parameter);
void hkl_parameter_value_set_smallest_in_range(HklParameter *parameter);
double hkl_parameter_value_get_closest(HklParameter *parameter, HklParameter *ref);

static void new(void)
{
	HklParameter *axis;
	static HklVector v = {{1, 0, 0}};
	double min, max;

	axis = hkl_parameter_new_axis("omega", &v);

	is_string("omega", hkl_parameter_name_get(axis), __func__);
	hkl_parameter_min_max_get(axis, &min, &max, HKL_UNIT_DEFAULT);
	is_double(-M_PI, min, HKL_EPSILON, __func__);
	is_double(M_PI, max, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(axis, HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);
	ok(TRUE == hkl_parameter_fit_get(axis), __func__);

	hkl_parameter_free(axis);
}

static void get_quaternions(void)
{
	static HklVector v_ref = {{1, 0, 0}};
	static HklQuaternion q1_ref = {{1, 0, 0, 0}};
	static HklQuaternion q2_ref = {{M_SQRT1_2, -M_SQRT1_2, 0, 0}};
	HklParameter *axis;

	axis = hkl_parameter_new_axis("omega", &v_ref);

	is_quaternion(&q1_ref, hkl_parameter_quaternion_get(axis), __func__);

	ok(TRUE == hkl_parameter_value_set(axis, -M_PI_2, HKL_UNIT_DEFAULT, NULL), __func__);
	is_quaternion(&q2_ref, hkl_parameter_quaternion_get(axis), __func__);

	hkl_parameter_free(axis);
}

static void copy(void)
{
	static HklVector v = {{1, 0, 0}};
	static HklQuaternion q_ref = {{M_SQRT1_2, -M_SQRT1_2, 0, 0}};
	HklParameter *axis;
	HklParameter *copy;
	double min, max;


	axis = hkl_parameter_new_axis("omega", &v);
	ok(TRUE == hkl_parameter_value_set(axis, -M_PI_2, HKL_UNIT_DEFAULT, NULL), __func__);

	copy = hkl_parameter_new_copy(axis);

	is_string("omega", hkl_parameter_name_get(copy), __func__);
	hkl_parameter_min_max_get(copy, &min, &max, HKL_UNIT_DEFAULT);
	is_double(-M_PI, min, HKL_EPSILON, __func__);
	is_double(M_PI, max, HKL_EPSILON, __func__);
	is_double(-M_PI_2, hkl_parameter_value_get(copy, HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);
	ok(TRUE == hkl_parameter_fit_get(copy), __func__);

	is_quaternion(&q_ref, hkl_parameter_quaternion_get(copy), __func__);

	hkl_parameter_free(axis);
	hkl_parameter_free(copy);
}

static void is_valid(void)
{
	static HklVector v = {{1, 0, 0}};
	HklParameter *axis1;

	axis1 = hkl_parameter_new_axis("omega", &v);

	ok(TRUE == hkl_parameter_value_set(axis1, 45, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_is_valid(axis1), __func__);

	/* change the range of axis1 */
	ok(TRUE == hkl_parameter_min_max_set(axis1, -270, 0, HKL_UNIT_USER, NULL), __func__);
	ok(FALSE == hkl_parameter_is_valid(axis1), __func__);

	ok(TRUE == hkl_parameter_value_set(axis1, -45, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_is_valid(axis1), __func__);

	ok(TRUE == hkl_parameter_min_max_set(axis1, 350, 450, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_value_set(axis1, 45, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_is_valid(axis1), __func__);
	ok(TRUE == hkl_parameter_value_set(axis1, -45, HKL_UNIT_USER, NULL), __func__);
	ok(FALSE == hkl_parameter_is_valid(axis1), __func__);

	ok(TRUE == hkl_parameter_min_max_set(axis1, -10, 90, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_value_set(axis1, 405, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_is_valid(axis1), __func__);
	ok(TRUE == hkl_parameter_value_set(axis1, -405, HKL_UNIT_USER, NULL), __func__);
	ok(FALSE == hkl_parameter_is_valid(axis1), __func__);

	hkl_parameter_free(axis1);
}

static void set_value_smallest_in_range(void)
{
	HklParameter *axis;
	static HklVector v = {{1, 0, 0}};

	axis = hkl_parameter_new_axis("omega", &v);

	ok(TRUE == hkl_parameter_min_max_set(axis, -190, 190, HKL_UNIT_USER, NULL), __func__);

	ok(TRUE == hkl_parameter_value_set(axis, 185, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-175., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis, 545, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-175., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis, -185, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-185., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis, 175, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-185., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis, 190, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-170., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis, -190, HKL_UNIT_USER, NULL), __func__);
	hkl_parameter_value_set_smallest_in_range(axis);
	is_double(-190., hkl_parameter_value_get(axis, HKL_UNIT_USER), HKL_EPSILON, __func__);

	hkl_parameter_free(axis);
}

static void get_value_closest(void)
{
	HklParameter *axis1, *axis2;
	static HklVector v = {{1, 0, 0}};

	axis1 = hkl_parameter_new_axis("omega", &v);
	axis2 = hkl_parameter_new_axis("omega", &v);

	ok(TRUE == hkl_parameter_value_set(axis1, 0, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_value_set(axis2, 0, HKL_UNIT_USER, NULL), __func__);
	is_double(0., hkl_parameter_value_get_closest(axis1,
						      axis2),
		  HKL_EPSILON, __func__);

	/* change the range of axis1 */
	ok(TRUE == hkl_parameter_min_max_set(axis1, -270, 180, HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_parameter_value_set(axis1, 100, HKL_UNIT_USER, NULL), __func__);

	ok(TRUE == hkl_parameter_value_set(axis2, -75, HKL_UNIT_USER, NULL), __func__);
	is_double(100 * HKL_DEGTORAD, hkl_parameter_value_get_closest(axis1,
								      axis2),
		  HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_value_set(axis2, -85, HKL_UNIT_USER, NULL), __func__);
	is_double(-260 * HKL_DEGTORAD, hkl_parameter_value_get_closest(axis1,
								       axis2),
		  HKL_EPSILON, __func__);

	hkl_parameter_free(axis1);
	hkl_parameter_free(axis2);
}

int main(void)
{
	plan(53);

	new();
	get_quaternions();
	copy();
	is_valid();
	set_value_smallest_in_range();
	get_value_closest();

	return 0;
}
