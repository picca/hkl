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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

#include "hkl-parameter-private.h"

static void new(void)
{
	HklParameter *p;

	ok(NULL == hkl_parameter_new("", "", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", "", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", "", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", "", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);

	ok(NULL == hkl_parameter_new("toto", "", 1, 2, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_length_nm), __func__);

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	ok(0 == !p, __func__);
	is_double(1., p->range.min, HKL_EPSILON, __func__);
	is_double(2., p->_value, HKL_EPSILON, __func__);
	is_double(3., p->range.max, HKL_EPSILON, __func__);
	ok(FALSE == p->fit, __func__);
	ok(TRUE == p->changed, __func__);
	ok(&hkl_unit_angle_rad == p->unit, __func__);
	ok(&hkl_unit_angle_deg == p->punit, __func__);

	hkl_parameter_free(p);
}

static void new_copy(void)
{
	HklParameter *copy, *p;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	copy = hkl_parameter_new_copy(p);

	ok(copy->name == p->name, __func__);
	ok(copy->description == p->description, __func__);
	is_double(copy->range.min, p->range.min, HKL_EPSILON, __func__);
	is_double(copy->_value, p->_value, HKL_EPSILON, __func__);
	is_double(copy->range.max, p->range.max, HKL_EPSILON, __func__);
	ok(copy->fit == p->fit, __func__);
	ok(copy->changed == p->changed, __func__);
	ok(&hkl_unit_angle_rad == copy->unit, __func__);
	ok(&hkl_unit_angle_deg == copy->punit, __func__);

	hkl_parameter_free(copy);
	hkl_parameter_free(p);
}

static void init(void)
{
	HklParameter *p;

	ok(NULL == hkl_parameter_new("", "no description", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", "no description", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", "no description", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", "no description", 2, 1, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", "no description", 1, 2, 3,
				     FALSE, TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_length_nm), __func__);
	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	ok(NULL != p, __func__);

	hkl_parameter_free(p);
}

static void is_valid(void)
{
	HklParameter *p;
	GError *error;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	ok(TRUE == hkl_parameter_is_valid(p), __func__);

	error= NULL;
	ok(TRUE == hkl_parameter_value_set(p, 10, HKL_UNIT_DEFAULT, &error), __func__);
	ok(error == NULL, __func__);
	ok(FALSE == hkl_parameter_is_valid(p), __func__);

	hkl_parameter_free(p);
}

static void min_max(void)
{
	HklParameter *p;
	double min, max;
	GError *error;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1, min, HKL_EPSILON, __func__);
	is_double(3, max, HKL_EPSILON, __func__);

	ok(TRUE == hkl_parameter_min_max_set(p, 1.1, 4, HKL_UNIT_DEFAULT, NULL), __func__);
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	error = NULL;
	ok(FALSE == hkl_parameter_min_max_set(p, 4, 1, HKL_UNIT_DEFAULT, &error), __func__);
	ok(error != NULL, __func__);
	g_clear_error(&error);

	/* nothing should have changed */
	hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_DEFAULT);
	is_double(1.1, min, HKL_EPSILON, __func__);
	is_double(4, max, HKL_EPSILON, __func__);

	hkl_parameter_free(p);
}

static void getter(void)
{
	HklParameter *p;

	p = hkl_parameter_new("toto", "no description", 1, 2, 3,
			      FALSE, TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	ok(NULL == hkl_parameter_axis_v_get(p), __func__);
	ok(NULL == hkl_parameter_quaternion_get(p), __func__);
	ok(NULL != hkl_parameter_description_get(p), __func__);

	hkl_parameter_free(p);
}

int main(int argc, char** argv)
{
	plan(44);

	new();
	new_copy();
	init();
	is_valid();
	min_max();
	getter();
	return 0;
}
