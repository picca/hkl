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

#define _GNU_SOURCE
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

#include "hkl-interval-private.h"

static void cmp(void)
{
	HklInterval interval_ref = {-1, 1};
	HklInterval interval;

	interval = interval_ref;
	ok(FALSE == hkl_interval_cmp(&interval_ref, &interval), __func__);
}

static void plus_interval(void)
{
	HklInterval i_ref = {-2, 8};
	HklInterval i1 = {-1, 4};
	HklInterval i2 = {-1, 4};

	hkl_interval_plus_interval(&i1, &i2);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i1), __func__);
}

static void plus_double(void)
{
	HklInterval i_ref = {-1, 9};
	HklInterval i1 = {-2, 8};

	hkl_interval_plus_double(&i1, 1);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i1), __func__);
}

static void times_interval(void)
{
	HklInterval i_ref = {-9, 36};
	HklInterval i1 = {-1, 9};
	HklInterval i2 = {-1, 4};

	hkl_interval_times_interval(&i1, &i2);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i1), __func__);
}

static void times_double(void)
{
	HklInterval i_ref = {-108, 27};
	HklInterval i1 = {-9, 36};

	hkl_interval_times_double(&i1, -3);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i1), __func__);
}

static void divides_double(void)
{
	HklInterval i_ref = {-9, 36};
	HklInterval i1 = {-108, 27};

	hkl_interval_divides_double(&i1, -3);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i1), __func__);
}

static void contain_zero(void)
{
	HklInterval i1 = {-9, 36};
	HklInterval i2 = {-108, -27};

	ok(TRUE == hkl_interval_contain_zero(&i1), __func__);
	ok(FALSE == hkl_interval_contain_zero(&i2), __func__);
}

static void cosinus(void)
{
	/* we will test all 16 cases. */
	/* we devide the trigonometric circle in 4 from quaters */
	/*  0 [0, pi/2[ */
	/*  1 [pi/2, pi[ */
	/*  2 [pi, 3*pi/2[ */
	/*  3 [3*pi/2, 2*pi[ */
	HklInterval i_ref;
	HklInterval i;
	double min;
	double max;

#define COS(a, b, min_ref, max_ref)  do {				\
		i.min = min = a * HKL_DEGTORAD;				\
		i.max = max = b * HKL_DEGTORAD;				\
		i_ref.min = min_ref;					\
		i_ref.max = max_ref;					\
		hkl_interval_cos(&i);					\
		ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);	\
	} while(0)

	/* 1st max(0) */
	/* min(0) */
	COS(10, 14, cos(max), cos(min));
	/* min(3) */
	COS(-15, 14,cos(min), 1);
	/* min(2) */
	COS(-95, 14, cos(min), 1);
	/* min(1) */
	COS(-215, 14, -1, 1);

	/* 2nd max(1) */
	/* min(0) */
	COS(10, 100, cos(max), cos(min));
	/* min(3) */
	COS(-20, 100, cos(max), 1);
	/* min(2) */
	COS(-110, 100, cos(min), 1);
	COS(-95, 100, cos(max), 1);
	/* min(1) */
	COS(-190, 100,-1, 1);

	/* 3rd max(2) */
	/* min(0) */
	COS(10, 190, -1, cos(min));
	/* min(3) */
	COS(95, 190, -1, cos(min));
	COS(175, 190, -1, cos(max));
	/* min(2) */
	COS(185, 190, cos(min), cos(max));
	COS(-95, 190, -1, 1);
	/* min(1) */
	COS(-45, 190, -1, 1);

	/* 4th max(3) */
	/* min(0) */
	COS(-350, -30, -1, cos(min));
	COS(-310, -30, -1, cos(max));
	/* min(3) */
	COS(-40, -30, cos(min), cos(max));
	COS(-370, -30, -1, 1);
	/* min(2) */
	COS(-100, -30, cos(min), cos(max));
	/* min(1) */
	COS(-190, -30, -1, cos(max));
}

static void acosinus(void)
{
	HklInterval i_ref = {acos(.5), acos(-.5)};
	HklInterval i = {-.5, .5};

	hkl_interval_acos(&i);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);
}

static void sinus(void)
{
	/* we will test all 16 cases. */
	/* we devide the trigonometric circle in 4 from quaters */
	/* 0 [0, pi/2[ */
	/* 1 [pi/2, pi[ */
	/* 2 [pi, 3*pi/2[ */
	/* 3 [3*pi/2, 2*pi[ */
	HklInterval i_ref;
	HklInterval i;
	double min, max;

#define SIN(a,b, min_ref, max_ref)  do {				\
		i.min = min = a *  HKL_DEGTORAD;			\
		i.max = max = b *  HKL_DEGTORAD;			\
		i_ref.min = min_ref;					\
		i_ref.max = max_ref;					\
		hkl_interval_sin(&i);					\
		ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);	\
	} while(0)

	/* 1st max(0) */
	/*  min(0) */
	SIN(10, 14,sin(min), sin(max));
	SIN(-275, 14, -1, 1);
	/* min(3) */
	SIN(-15, 14, sin(min), sin(max));
	/* min(2) */
	SIN(-95, 14, -1, sin(max));
	/* min(1) */
	SIN(-185, 14, -1, sin(max));
	SIN(-215, 14, -1, sin(min));

	/* 2nd max(1) */
	/* min(0) */
	SIN(10, 100, sin(min), 1);
	SIN(85, 100, sin(max), 1);
	/* min(3) */
	SIN(-20, 100, sin(min), 1);
	/* min(2) */
	SIN(-110, 100, -1, 1);
	/* min(1) */
	SIN(-190, 100, -1, 1);
	SIN(95, 100, sin(max), sin(min));

	/* 3rd max(2) */
	/* min(0) */
	SIN(10, 190, sin(max), 1);
	/* min(3) */
	SIN(95, 190, sin(max), sin(min));
	/* min(2) */
	SIN(-95, 190, -1, 1);
	SIN(185, 190, sin(max), sin(min));
	/* min(1) */
	SIN(-5, 190, sin(max), 1);
	SIN(-45, 190, sin(min), 1);

	/* 4th max(3) */
	/* min(0); */
	SIN(-350, -30, -1, 1);
	/* min(3); */
	SIN(-40, -30, sin(min), sin(max));
	SIN(-370, -30, -1, 1);
	/* min(2); */
	SIN(-100, -30, -1, sin(max));
	SIN(-170, -30, -1, sin(min));
	/* min(1); */
	SIN(-190, -30, -1, sin(min));
}

static void asinus(void)
{
	HklInterval i_ref = {asin(-.5), asin(.5)};
	HklInterval i = {-.5, .5};

	hkl_interval_asin(&i);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);
}

static void tangeante(void)
{
	HklInterval i;
	HklInterval i_ref;
	double min, max;

#define TAN(a,b, min_ref, max_ref)  do {				\
		i.min = min = a *  HKL_DEGTORAD;			\
		i.max = max = b *  HKL_DEGTORAD;			\
		i_ref.min = min_ref;					\
		i_ref.max = max_ref;					\
		hkl_interval_tan(&i);					\
		ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);	\
	} while(0)

	TAN(-100, -89, -INFINITY, INFINITY);

	/*
	// The limit case is not yet ok
	TAN(-100, 11, 12, -90);
	r_ref.set(-hklconstantmathinfinity, tan(current), tan(consign), hklconstantmathinfinity);
	ok(r_ref, r);
	*/
}

static void atangeante(void)
{
	HklInterval i_ref = {atan(-10.), atan(10)};
	HklInterval i = {-10, 10};

	hkl_interval_atan(&i);
	ok(FALSE == hkl_interval_cmp(&i_ref, &i), __func__);
}

static void length(void)
{
	HklInterval interval;

	interval.min = 10;
	interval.max = 11;
	is_double(1., hkl_interval_length(&interval), HKL_EPSILON, __func__);

	interval.min = -11;
	interval.max = -10;
	is_double(1., hkl_interval_length(&interval), HKL_EPSILON, __func__);
}

int main(int argc, char** argv)
{
	plan(59);

	cmp();
	plus_interval();
	plus_double();
	times_interval();
	times_double();
	divides_double();
	contain_zero();
	cosinus();
	acosinus();
	sinus();
	asinus();
	tangeante();
	atangeante();
	length();

	return 0;
}
