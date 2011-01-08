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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>
#include <tap/basic.h>

static void new(void)
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	is_string("omega", ((HklParameter *)axis)->name, __func__);
	is_double(-M_PI, ((HklParameter *)axis)->range.min, HKL_EPSILON, __func__);
	is_double(M_PI, ((HklParameter *)axis)->range.max, HKL_EPSILON, __func__);
	is_double(0., ((HklParameter *)axis)->value, HKL_EPSILON, __func__);
	ok(HKL_TRUE == ((HklParameter *)axis)->fit, __func__);
	ok(HKL_TRUE == ((HklParameter *)axis)->changed, __func__);

	hkl_axis_free(axis);
}

static void get_quaternions(void)
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};
	HklQuaternion q;

	axis = hkl_axis_new("omega", &v);

	hkl_axis_get_quaternion(axis, &q);
	is_double(1., q.data[0], HKL_EPSILON, __func__);
	is_double(0., q.data[1], HKL_EPSILON, __func__);
	is_double(0., q.data[2], HKL_EPSILON, __func__);
	is_double(0., q.data[3], HKL_EPSILON, __func__);

	((HklParameter *)axis)->value = -M_PI_2;
	hkl_axis_get_quaternion(axis, &q);
	is_double(1./sqrt(2.), q.data[0], HKL_EPSILON, __func__);
	is_double(-1./sqrt(2.), q.data[1], HKL_EPSILON, __func__);
	is_double(0., q.data[2], HKL_EPSILON, __func__);
	is_double(0., q.data[3], HKL_EPSILON, __func__);

	hkl_axis_free(axis);
}

static void is_value_compatible_with_range(void)
{
	HklAxis *axis1;
	HklVector v = {{1, 0, 0}};

	axis1 = hkl_axis_new("omega", &v);

	hkl_axis_set_value_unit(axis1, 45);
	ok(HKL_TRUE == hkl_axis_is_value_compatible_with_range(axis1), __func__);

	/* change the range of axis1 */
	hkl_axis_set_range_unit(axis1, -270, 0);
	ok(HKL_FALSE == hkl_axis_is_value_compatible_with_range(axis1), __func__);

	hkl_axis_set_value_unit(axis1, -45);
	ok(HKL_TRUE == hkl_axis_is_value_compatible_with_range(axis1), __func__);

	hkl_axis_set_range_unit(axis1, 350, 450);
	hkl_axis_set_value_unit(axis1, 45);
	ok(HKL_TRUE == hkl_axis_is_value_compatible_with_range(axis1), __func__);
	hkl_axis_set_value_unit(axis1, -45);
	ok(HKL_FALSE == hkl_axis_is_value_compatible_with_range(axis1), __func__);

	hkl_axis_set_range_unit(axis1, -10, 90);
	hkl_axis_set_value_unit(axis1, 405);
	ok(HKL_TRUE == hkl_axis_is_value_compatible_with_range(axis1), __func__);
	hkl_axis_set_value_unit(axis1, -405);
	ok(HKL_FALSE == hkl_axis_is_value_compatible_with_range(axis1), __func__);

	hkl_axis_free(axis1);
}

static void set_value_smallest_in_range(void)
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	hkl_axis_set_range_unit(axis, -190, 190);

	hkl_axis_set_value_unit(axis, 185);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-175., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis, 545);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-175., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis, -185);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-185., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis, 175);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-185., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis, 190);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-170., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis, -190);
	hkl_axis_set_value_smallest_in_range(axis);
	is_double(-190., hkl_axis_get_value_unit(axis), HKL_EPSILON, __func__);

	hkl_axis_free(axis);
}

static void get_value_closest(void)
{
	HklAxis *axis1, *axis2;
	HklVector v = {{1, 0, 0}};

	axis1 = hkl_axis_new("omega", &v);
	axis2 = hkl_axis_new("omega", &v);

	hkl_axis_set_value_unit(axis1, 0);
	hkl_axis_set_value_unit(axis2, 0);
	is_double(0., hkl_axis_get_value_closest(axis1, axis2), HKL_EPSILON, __func__);

	/* change the range of axis1 */
	hkl_axis_set_range_unit(axis1, -270, 180);
	hkl_axis_set_value_unit(axis1, 100);

	hkl_axis_set_value_unit(axis2, -75);
	is_double(100*HKL_DEGTORAD, hkl_axis_get_value_closest(axis1, axis2), HKL_EPSILON, __func__);

	hkl_axis_set_value_unit(axis2, -85);
	is_double(-260*HKL_DEGTORAD, hkl_axis_get_value_closest(axis1, axis2), HKL_EPSILON, __func__);

	hkl_axis_free(axis1);
	hkl_axis_free(axis2);
}

int main(int argc, char** argv)
{
	plan(30);

	new();
	get_quaternions();
	is_value_compatible_with_range();
	set_value_smallest_in_range();
	get_value_closest();

	return 0;
}
