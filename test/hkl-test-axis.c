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
#include <string.h>
#include <math.h>

#include <hkl/hkl-axis.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME axis

HKL_TEST_SUITE_FUNC( new )
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	HKL_ASSERT_EQUAL(0, strcmp("omega", ((HklParameter *)axis)->name));
	HKL_ASSERT_DOUBLES_EQUAL(-M_PI, ((HklParameter *)axis)->range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(M_PI, ((HklParameter *)axis)->range.max, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., ((HklParameter *)axis)->value, HKL_EPSILON);
	HKL_ASSERT_EQUAL(HKL_FALSE, ((HklParameter *)axis)->not_to_fit);
	HKL_ASSERT_EQUAL(HKL_TRUE, ((HklParameter *)axis)->changed);

	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( get_quaternions )
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};
	HklQuaternion q;

	axis = hkl_axis_new("omega", &v);

	hkl_axis_get_quaternion(axis, &q);
	HKL_ASSERT_DOUBLES_EQUAL(1., q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[3], HKL_EPSILON);

	((HklParameter *)axis)->value = -M_PI_2;
	hkl_axis_get_quaternion(axis, &q);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2.), q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(-1./sqrt(2.), q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[3], HKL_EPSILON);

	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( get_quaternions );

HKL_TEST_SUITE_END
