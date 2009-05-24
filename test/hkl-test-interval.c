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
#define _GNU_SOURCE
#include <hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME interval

HKL_TEST_SUITE_FUNC(cmp)
{
	HklInterval interval_ref = {-1, 1};
	HklInterval interval;

	interval = interval_ref;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&interval_ref, &interval));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(plus_interval)
{
	HklInterval i_ref = {-2, 8};
	HklInterval i1 = {-1, 4};
	HklInterval i2 = {-1, 4};

	hkl_interval_plus_interval(&i1, &i2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(plus_double)
{
	HklInterval i_ref = {-1, 9};
	HklInterval i1 = {-2, 8};

	hkl_interval_plus_double(&i1, 1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_interval)
{
	HklInterval i_ref = {-9, 36};
	HklInterval i1 = {-1, 9};
	HklInterval i2 = {-1, 4};

	hkl_interval_times_interval(&i1, &i2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_double)
{
	HklInterval i_ref = {-108, 27};
	HklInterval i1 = {-9, 36};

	hkl_interval_times_double(&i1, -3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(divides_double)
{
	HklInterval i_ref = {-9, 36};
	HklInterval i1 = {-108, 27};

	hkl_interval_divides_double(&i1, -3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(contain_zero)
{
	HklInterval i1 = {-9, 36};
	HklInterval i2 = {-108, -27};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_contain_zero(&i1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_interval_contain_zero(&i2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cos)
{
	// we will test all 16 cases.
	// we devide the trigonometric circle in 4 from quaters
	// 0 [0, pi/2[
	// 1 [pi/2, pi[
	// 2 [pi, 3*pi/2[
	// 3 [3*pi/2, 2*pi[
	HklInterval i_ref;
	HklInterval i;
	double min;
	double max;

#define COS(a, b, min_ref, max_ref)  do {\
	i.min = min = a * HKL_DEGTORAD;\
	i.max = max = b * HKL_DEGTORAD;\
	i_ref.min = min_ref;\
	i_ref.max = max_ref;\
	hkl_interval_cos(&i);\
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));\
} while(0)

	// 1st max(0)
	// min(0);
	COS(10, 14, cos(max), cos(min));
	// min(3);
	COS(-15, 14,cos(min), 1);
	// min(2);
	COS(-95, 14, cos(min), 1);
	// min(1);
	COS(-215, 14, -1, 1);

	// 2nd max(1)
	// min(0);
	COS(10, 100, cos(max), cos(min));
	// min(3);
	COS(-20, 100, cos(max), 1);
	// min(2);
	COS(-110, 100, cos(min), 1);
	COS(-95, 100, cos(max), 1);
	// min(1);
	COS(-190, 100,-1, 1);

	// 3rd max(2)
	// min(0);
	COS(10, 190, -1, cos(min));
	// min(3);
	COS(95, 190, -1, cos(min));
	COS(175, 190, -1, cos(max));
	// min(2);
	COS(185, 190, cos(min), cos(max));
	COS(-95, 190, -1, 1);
	// min(1);
	COS(-45, 190, -1, 1);

	// 4th max(3)
	// min(0);
	COS(-350, -30, -1, cos(min));
	COS(-310, -30, -1, cos(max));
	// min(3);
	COS(-40, -30, cos(min), cos(max));
	COS(-370, -30, -1, 1);
	// min(2);
	COS(-100, -30, cos(min), cos(max));
	// min(1);
	COS(-190, -30, -1, cos(max));

	return HKL_TEST_PASS;
	}

HKL_TEST_SUITE_FUNC(acos)
{
	HklInterval i_ref = {acos(.5), acos(-.5)};
	HklInterval i = {-.5, .5};

	hkl_interval_acos(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(sin)
{
	// we will test all 16 cases.
	// we devide the trigonometric circle in 4 from quaters
	// 0 [0, pi/2[
	// 1 [pi/2, pi[
	// 2 [pi, 3*pi/2[
	// 3 [3*pi/2, 2*pi[
	HklInterval i_ref;
	HklInterval i;
	double min, max;

#define SIN(a,b, min_ref, max_ref)  do {\
	i.min = min = a *  HKL_DEGTORAD;\
	i.max = max = b *  HKL_DEGTORAD;\
	i_ref.min = min_ref;\
	i_ref.max = max_ref;\
	hkl_interval_sin(&i);\
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));\
} while(0)

	// 1st max(0)
	// min(0);
	SIN(10, 14,sin(min), sin(max));
	SIN(-275, 14, -1, 1);
	// min(3);
	SIN(-15, 14, sin(min), sin(max));
	// min(2);
	SIN(-95, 14, -1, sin(max));
	// min(1);
	SIN(-185, 14, -1, sin(max));
	SIN(-215, 14, -1, sin(min));

	// 2nd max(1)
	// min(0);
	SIN(10, 100, sin(min), 1);
	SIN(85, 100, sin(max), 1);
	// min(3);
	SIN(-20, 100, sin(min), 1);
	// min(2);
	SIN(-110, 100, -1, 1);
	// min(1);
	SIN(-190, 100, -1, 1);
	SIN(95, 100, sin(max), sin(min));

	// 3rd max(2)
	// min(0);
	SIN(10, 190, sin(max), 1);
	// min(3);
	SIN(95, 190, sin(max), sin(min));
	// min(2);
	SIN(-95, 190, -1, 1);
	SIN(185, 190, sin(max), sin(min));
	// min(1);
	SIN(-5, 190, sin(max), 1);
	SIN(-45, 190, sin(min), 1);

	// 4th max(3)
	// min(0);
	SIN(-350, -30, -1, 1);
	// min(3);
	SIN(-40, -30, sin(min), sin(max));
	SIN(-370, -30, -1, 1);
	// min(2);
	SIN(-100, -30, -1, sin(max));
	SIN(-170, -30, -1, sin(min));
	// min(1);
	SIN(-190, -30, -1, sin(min));

	return HKL_TEST_PASS;
	}

HKL_TEST_SUITE_FUNC(asin)
{
	HklInterval i_ref = {asin(-.5), asin(.5)};
	HklInterval i = {-.5, .5};

	hkl_interval_asin(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(tan)
{
	HklInterval i;
	HklInterval i_ref;
	double min, max;

#define TAN(a,b, min_ref, max_ref)  do {\
	i.min = min = a *  HKL_DEGTORAD;\
	i.max = max = b *  HKL_DEGTORAD;\
	i_ref.min = min_ref;\
	i_ref.max = max_ref;\
	hkl_interval_tan(&i);\
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));\
} while(0)

	TAN(-100, -89, -INFINITY, INFINITY);

	/*
	// The limit case is not yet ok
	TAN(-100, 11, 12, -90);
	r_ref.set(-hklconstantmathinfinity, tan(current), tan(consign), hklconstantmathinfinity);
	HKL_ASSERT_EQUAL(r_ref, r);
	*/

	return HKL_TEST_PASS;
	}

HKL_TEST_SUITE_FUNC(atan)
{
	HklInterval i_ref = {atan(-10.), atan(10)};
	HklInterval i = {-10, 10};

	hkl_interval_atan(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(length)
{
	HklInterval interval;

	interval.min = 10;
	interval.max = 11;
	HKL_ASSERT_DOUBLES_EQUAL(1, hkl_interval_length(&interval), HKL_EPSILON);

	interval.min = -11;
	interval.max = -10;
	HKL_ASSERT_DOUBLES_EQUAL(1, hkl_interval_length(&interval), HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( cmp );
HKL_TEST( plus_interval );
HKL_TEST( plus_double );
HKL_TEST( times_interval );
HKL_TEST( times_double );
HKL_TEST( divides_double );
HKL_TEST( contain_zero );
HKL_TEST( cos );
HKL_TEST( acos );
HKL_TEST( sin );
HKL_TEST( asin );
HKL_TEST( tan );
HKL_TEST( atan );
HKL_TEST( length );

HKL_TEST_SUITE_END
