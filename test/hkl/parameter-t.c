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
	HklParameter *p;

	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);

	ok(NULL == hkl_parameter_new("toto", 1, 2, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_length_nm), __func__);

	p = hkl_parameter_new("toto", 1, 2, 3,
			      HKL_FALSE, HKL_TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	ok(0 == !p, __func__);
	is_double(1., p->range.min, HKL_EPSILON, __func__);
	is_double(2., p->value, HKL_EPSILON, __func__);
	is_double(3., p->range.max, HKL_EPSILON, __func__);
	ok(HKL_FALSE == p->fit, __func__);
	ok(HKL_TRUE == p->changed, __func__);
	ok(&hkl_unit_angle_rad == p->unit, __func__);
	ok(&hkl_unit_angle_deg == p->punit, __func__);

	hkl_parameter_free(p);
}

static void new_copy(void)
{
	HklParameter *copy, *p;

	p = hkl_parameter_new("toto", 1, 2, 3,
			      HKL_FALSE, HKL_TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	copy = hkl_parameter_new_copy(p);

	ok(copy->name == p->name, __func__);
	is_double(copy->range.min, p->range.min, HKL_EPSILON, __func__);
	is_double(copy->value, p->value, HKL_EPSILON, __func__);
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

	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", 2, 1, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);
	ok(NULL == hkl_parameter_new("toto", 1, 2, 3,
				     HKL_FALSE, HKL_TRUE,
				     &hkl_unit_angle_rad, &hkl_unit_length_nm), __func__);
	ok(NULL!= hkl_parameter_new("toto", 1, 2, 3,
				    HKL_FALSE, HKL_TRUE,
				    &hkl_unit_angle_rad, &hkl_unit_angle_deg), __func__);

	hkl_parameter_free(p);
}

static void is_valid(void)
{
	HklParameter *p;

	p = hkl_parameter_new("toto", 1, 2, 3,
			      HKL_FALSE, HKL_TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	ok(HKL_TRUE == hkl_parameter_is_valid(p), __func__);

	hkl_parameter_set_value(p, 10);
	ok(HKL_FALSE == hkl_parameter_is_valid(p), __func__);

	hkl_parameter_free(p);
}

int main(int argc, char** argv)
{
	plan(29);

	new();
	new_copy();
	init();
	is_valid();

	return 0;
}
