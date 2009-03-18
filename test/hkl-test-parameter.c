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
#include <math.h>

#include <hkl/hkl-parameter.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME parameter

HKL_TEST_SUITE_FUNC(new)
{
	HklParameter *p;

	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_parameter_new("toto", 2, 1, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_angle_deg));

	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_parameter_new("toto", 1, 2, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_length_nm));

	p = hkl_parameter_new("toto", 1, 2, 3,
			      HKL_TRUE, HKL_TRUE,
			      &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	HKL_ASSERT_EQUAL(0, !p);
	HKL_ASSERT_DOUBLES_EQUAL(1., p->range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(2., p->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(3., p->range.max, HKL_EPSILON);
	HKL_ASSERT_EQUAL(HKL_TRUE, p->not_to_fit);
	HKL_ASSERT_EQUAL(HKL_TRUE, p->changed);
	HKL_ASSERT_POINTER_EQUAL(&hkl_unit_angle_rad, p->unit);
	HKL_ASSERT_POINTER_EQUAL(&hkl_unit_angle_deg, p->punit);

	hkl_parameter_free(p);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklParameter *copy, p;

	hkl_parameter_init(&p, "toto", 1, 2, 3,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	copy = hkl_parameter_new_copy(&p);

	HKL_ASSERT_POINTER_EQUAL(copy->name, p.name);
	HKL_ASSERT_DOUBLES_EQUAL(copy->range.min, p.range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(copy->value, p.value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(copy->range.max, p.range.max, HKL_EPSILON);
	HKL_ASSERT_EQUAL(copy->not_to_fit, p.not_to_fit);
	HKL_ASSERT_EQUAL(copy->changed, p.changed);
	HKL_ASSERT_POINTER_EQUAL(&hkl_unit_angle_rad, copy->unit);
	HKL_ASSERT_POINTER_EQUAL(&hkl_unit_angle_deg, copy->punit);

	hkl_parameter_free(copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(init)
{
	HklParameter p;

	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&p, "", 2, 1, 3,
						      HKL_TRUE, HKL_TRUE,
						      &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&p, "", 2, 1, 3,
						      HKL_TRUE, HKL_TRUE,
						      &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&p, "", 2, 1, 3,
						      HKL_TRUE, HKL_TRUE,
						      &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&p, "toto", 2, 1, 3,
						      HKL_TRUE, HKL_TRUE,
						      &hkl_unit_angle_rad, &hkl_unit_angle_deg));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&p, "toto", 1, 2, 3,
						      HKL_TRUE, HKL_TRUE,
						      &hkl_unit_angle_rad, &hkl_unit_length_nm));
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_parameter_init(&p, "toto", 1, 2, 3,
							 HKL_TRUE, HKL_TRUE,
							 &hkl_unit_angle_rad, &hkl_unit_angle_deg));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( new_copy );
HKL_TEST( init );

HKL_TEST_SUITE_END
