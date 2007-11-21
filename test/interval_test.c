#define _GNU_SOURCE
#include <math.h>

#include <hkl/config.h>
#include <hkl/interval.h>

#include "test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME interval

HKL_TEST_SUITE_FUNC(cmp)
{
	struct hkl_interval interval_ref = {-1, 1};
	struct hkl_interval interval;

	interval = interval_ref;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&interval_ref, &interval));
}

HKL_TEST_SUITE_FUNC(plus_interval)
{
	struct hkl_interval i_ref = {-2, 8};
	struct hkl_interval i1 = {-1, 4};
	struct hkl_interval i2 = {-1, 4};

	hkl_interval_plus_interval(&i1, &i2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));
}

HKL_TEST_SUITE_FUNC(plus_double)
{
	struct hkl_interval i_ref = {-1, 9};
	struct hkl_interval i1 = {-2, 8};

	hkl_interval_plus_double(&i1, 1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));
}

HKL_TEST_SUITE_FUNC(times_interval)
{
	struct hkl_interval i_ref = {-9, 36};
	struct hkl_interval i1 = {-1, 9};
	struct hkl_interval i2 = {-1, 4};

	hkl_interval_times_interval(&i1, &i2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));
}

HKL_TEST_SUITE_FUNC(times_double)
{
	struct hkl_interval i_ref = {-108, 27};
	struct hkl_interval i1 = {-9, 36};

	hkl_interval_times_double(&i1, -3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));
}

HKL_TEST_SUITE_FUNC(divides_double)
{
	struct hkl_interval i_ref = {-9, 36};
	struct hkl_interval i1 = {-108, 27};

	hkl_interval_divides_double(&i1, -3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i1));
}

HKL_TEST_SUITE_FUNC(contain_zero)
{
	struct hkl_interval i1 = {-9, 36};
	struct hkl_interval i2 = {-108, -27};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_contain_zero(&i1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_interval_contain_zero(&i2));
}

HKL_TEST_SUITE_FUNC(cos)
{
	// we will test all 16 cases.
	// we devide the trigonometric circle in 4 from quaters
	// 0 [0, pi/2[
	// 1 [pi/2, pi[
	// 2 [pi, 3*pi/2[
	// 3 [3*pi/2, 2*pi[
	struct hkl_interval i_ref;
	struct hkl_interval i;
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
	}

HKL_TEST_SUITE_FUNC(acos)
{
	struct hkl_interval i_ref = {acos(.5), acos(-.5)};
	struct hkl_interval i = {-.5, .5};

	hkl_interval_acos(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));
}

HKL_TEST_SUITE_FUNC(sin)
{
	// we will test all 16 cases.
	// we devide the trigonometric circle in 4 from quaters
	// 0 [0, pi/2[
	// 1 [pi/2, pi[
	// 2 [pi, 3*pi/2[
	// 3 [3*pi/2, 2*pi[
	struct hkl_interval i_ref;
	struct hkl_interval i;
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
	}

HKL_TEST_SUITE_FUNC(asin)
{
	struct hkl_interval i_ref = {asin(-.5), asin(.5)};
	struct hkl_interval i = {-.5, .5};

	hkl_interval_asin(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));
}

HKL_TEST_SUITE_FUNC(tan)
{
	struct hkl_interval i;
	struct hkl_interval i_ref;
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
	}

HKL_TEST_SUITE_FUNC(atan)
{
	struct hkl_interval i_ref = {atan(-10.), atan(10)};
	struct hkl_interval i = {-10, 10};

	hkl_interval_atan(&i);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_interval_cmp(&i_ref, &i));
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

HKL_TEST_SUITE_END
