#include <cmath>
#include <config.h>
#include "interval_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( IntervalTest );

void
IntervalTest::setUp() {}

void
IntervalTest::tearDown() {}


void
IntervalTest::hkl_interval_cmp(void)
{
  static hkl_interval interval_ref = {-1, 1};
  hkl_interval interval;

  interval = interval_ref;
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&interval_ref, &interval));
}

void
IntervalTest::hkl_interval_plus_interval(void)
{
  static hkl_interval i_ref = {-2, 8};
  hkl_interval i1 = {-1, 4};
  hkl_interval i2 = {-1, 4};

  ::hkl_interval_plus_interval(&i1, &i2);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i1));
}

void
IntervalTest::hkl_interval_plus_double(void)
{
  static hkl_interval i_ref = {-1, 9};
  hkl_interval i1 = {-2, 8};

  ::hkl_interval_plus_double(&i1, 1);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i1));
}

void
    IntervalTest::hkl_interval_minus_interval(void)
    {

    }

void
    IntervalTest::hkl_interval_minus_double(void)
    {
    }

void
IntervalTest::hkl_interval_times_interval(void)
{
  static hkl_interval i_ref = {-9, 36};
  hkl_interval i1 = {-1, 9};
  hkl_interval i2 = {-1, 4};

  ::hkl_interval_times_interval(&i1, &i2);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i1));
}

void
IntervalTest::hkl_interval_times_double(void)
{
  static hkl_interval i_ref = {-108, 27};
  hkl_interval i1 = {-9, 36};

  ::hkl_interval_times_double(&i1, -3);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i1));
}

void
IntervalTest::hkl_interval_divides_double(void)
{
  static hkl_interval i_ref = {-9, 36};
  hkl_interval i1 = {-108, 27};

  ::hkl_interval_divides_double(&i1, -3);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i1));
}

void
IntervalTest::hkl_interval_contain_zero(void)
{
  static hkl_interval i1 = {-9, 36};
  static hkl_interval i2 = {-108, -27};

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_contain_zero(&i1));
  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_interval_contain_zero(&i2));
}

void
IntervalTest::hkl_interval_cos(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[
  // 1 [pi/2, pi[
  // 2 [pi, 3*pi/2[
  // 3 [3*pi/2, 2*pi[
  hkl_interval i_ref;
  hkl_interval i;
  double min;
  double max;

#define COS(a, b, min_ref, max_ref)  do {\
  i.min = min = a * HKL_DEGTORAD;\
  i.max = max = b * HKL_DEGTORAD;\
  i_ref.min = min_ref;\
  i_ref.max = max_ref;\
  ::hkl_interval_cos(&i);\
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));\
} while(0)

  // 1st max(0)
  // min(0);
  COS(10, 14, ::cos(max), ::cos(min));
  // min(3);
  COS(-15, 14,::cos(min), 1);
  // min(2);
  COS(-95, 14, ::cos(min), 1);
  // min(1);
  COS(-215, 14, -1, 1);
  
  // 2nd max(1)
  // min(0);
  COS(10, 100, ::cos(max), ::cos(min));
  // min(3);
  COS(-20, 100, ::cos(max), 1);
  // min(2);
  COS(-110, 100, ::cos(min), 1);
  COS(-95, 100, ::cos(max), 1);
  // min(1);
  COS(-190, 100,-1, 1);

  // 3rd max(2)
  // min(0);
  COS(10, 190, -1, ::cos(min));
  // min(3);
  COS(95, 190, -1, ::cos(min));
  COS(175, 190, -1, ::cos(max));
  // min(2);
  COS(185, 190, ::cos(min), ::cos(max));
  COS(-95, 190, -1, 1);
  // min(1);
  COS(-45, 190, -1, 1);

  // 4th max(3)
  // min(0);
  COS(-350, -30, -1, ::cos(min));
  COS(-310, -30, -1, ::cos(max));
  // min(3);
  COS(-40, -30, ::cos(min), ::cos(max));
  COS(-370, -30, -1, 1);
  // min(2);
  COS(-100, -30, ::cos(min), ::cos(max));
  // min(1);
  COS(-190, -30, -1, ::cos(max));
}

void
IntervalTest::hkl_interval_acos(void)
{
  static hkl_interval i_ref = {::acos(.5), ::acos(-.5)};
  hkl_interval i = {-.5, .5};

  ::hkl_interval_acos(&i);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));
}

void
IntervalTest::hkl_interval_sin(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[
  // 1 [pi/2, pi[
  // 2 [pi, 3*pi/2[
  // 3 [3*pi/2, 2*pi[
  hkl_interval i_ref;
  hkl_interval i;
  double min, max;

#define SIN(a,b, min_ref, max_ref)  do {\
  i.min = min = a *  HKL_DEGTORAD;\
  i.max = max = b *  HKL_DEGTORAD;\
  i_ref.min = min_ref;\
  i_ref.max = max_ref;\
  ::hkl_interval_sin(&i);\
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));\
} while(0)

  // 1st max(0)
  // min(0);
  SIN(10, 14,::sin(min), ::sin(max));
  SIN(-275, 14, -1, 1);
  // min(3);
  SIN(-15, 14, ::sin(min), ::sin(max));
  // min(2);
  SIN(-95, 14, -1, ::sin(max));
  // min(1);
  SIN(-185, 14, -1, ::sin(max));
  SIN(-215, 14, -1, ::sin(min));

  // 2nd max(1)
  // min(0);
  SIN(10, 100, ::sin(min), 1);
  SIN(85, 100, ::sin(max), 1);
  // min(3);
  SIN(-20, 100, ::sin(min), 1);
  // min(2);
  SIN(-110, 100, -1, 1);
  // min(1);
  SIN(-190, 100, -1, 1);
  SIN(95, 100, ::sin(max), ::sin(min));

  // 3rd max(2)
  // min(0);
  SIN(10, 190, ::sin(max), 1);
  // min(3);
  SIN(95, 190, ::sin(max), ::sin(min));
  // min(2);
  SIN(-95, 190, -1, 1);
  SIN(185, 190, ::sin(max), ::sin(min));
  // min(1);
  SIN(-5, 190, ::sin(max), 1);
  SIN(-45, 190, ::sin(min), 1);

  // 4th max(3)
  // min(0);
  SIN(-350, -30, -1, 1);
  // min(3);
  SIN(-40, -30, ::sin(min), ::sin(max));
  SIN(-370, -30, -1, 1);
  // min(2);
  SIN(-100, -30, -1, ::sin(max));
  SIN(-170, -30, -1, ::sin(min));
  // min(1);
  SIN(-190, -30, -1, ::sin(min));
}

void
IntervalTest::hkl_interval_asin(void)
{
  static hkl_interval i_ref = {::asin(-.5), ::asin(.5)};
  hkl_interval i = {-.5, .5};

  ::hkl_interval_asin(&i);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));
}

void
IntervalTest::hkl_interval_tan(void)
{
  hkl_interval i;
  hkl_interval i_ref;
  double min, max;

#define TAN(a,b, min_ref, max_ref)  do {\
  i.min = min = a *  HKL_DEGTORAD;\
  i.max = max = b *  HKL_DEGTORAD;\
  i_ref.min = min_ref;\
  i_ref.max = max_ref;\
  ::hkl_interval_tan(&i);\
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));\
} while(0)

  TAN(-100, -89, -INFINITY, INFINITY);

  /*
  // The limit case is not yet ok
  TAN(-100, 11, 12, -90);
  r_ref.set(-hkl::constant::math::infinity, ::tan(current), ::tan(consign), hkl::constant::math::infinity);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  */
}

void
IntervalTest::hkl_interval_atan(void)
{
  static hkl_interval i_ref = {::atan(-10.), ::atan(10)};
  hkl_interval i = {-10, 10};
  
  ::hkl_interval_atan(&i);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_interval_cmp(&i_ref, &i));
}
