#include <cmath>
#include <config.h>
#include "interval_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( IntervalTest );

void
IntervalTest::setUp()
{
  _interval = hkl::Interval();
}

void
IntervalTest::tearDown() {}

void
IntervalTest::Constructors()
{
  double min = -1.34;
  double max = 1.34;
  // default constructor
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, _interval.get_min(), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, _interval.get_max(), HKL_EPSILON);

  // 1st constructor
  CPPUNIT_ASSERT_THROW(hkl::Interval interval(max, min), hkl::HKLException);

  hkl::Interval interval(min, max);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(min, interval.get_min(), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(max, interval.get_max(), HKL_EPSILON);

  // copy constructor
  hkl::Interval interval2(interval);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(min, interval2.get_min(), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(max, interval2.get_max(), HKL_EPSILON);
}

void
IntervalTest::Equal()
{
  _interval = hkl::Interval(-1, 1);
  hkl::Interval interval(_interval);

  CPPUNIT_ASSERT_EQUAL(_interval, interval);
}

void
IntervalTest::GetSet()
{
  CPPUNIT_ASSERT_THROW(_interval.set_min(4), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_interval.set_max(-4), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(_interval.set_min(-3));
  CPPUNIT_ASSERT_NO_THROW(_interval.set_max(3));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-3, _interval.get_min(), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(3, _interval.get_max(), HKL_EPSILON);
}

void
IntervalTest::operators(void)
{
  //operator+=(Interval)
  hkl::Interval r1(-1, 4);
  hkl::Interval r2(-1, 4);
  hkl::Interval r_ref(-2, 8);
  r1 += r2;
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator+=(double)
  r1 += 1;
  r_ref.set(-1, 9);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator*=(interval)
  r1 *= r2;
  r_ref.set(-9, 36);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator*=(double)
  r1 *= -3;
  r_ref.set(-108, 27);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator/=(double)
  r1 /= -3;
  r_ref.set(-9, 36);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);
}

void
IntervalTest::cos(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[
  // 1 [pi/2, pi[
  // 2 [pi, 3*pi/2[
  // 3 [3*pi/2, 2*pi[
  hkl::Interval r;
  hkl::Interval r_ref;
  double min, max;

#define COS(a,b)  do {\
    min = a *  HKL_DEGTORAD;\
    max = b *  HKL_DEGTORAD;\
    r.set(min, max);\
    r.cos();\
} while(0)

  // 1st max(0)
  // min(0);
  COS(10, 14);
  r_ref.set(::cos(max), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-15, 14);
  r_ref.set(::cos(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-95, 14);
  r_ref.set(::cos(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-215, 14);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 2nd max(1)
  // min(0);
  COS(10, 100);
  r_ref.set(::cos(max), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-20, 100);
  r_ref.set(::cos(max), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-110, 100);
  r_ref.set(::cos(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-95, 100);
  r_ref.set(::cos(max), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-190, 100);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 3rd max(2)
  // min(0);
  COS(10, 190);
  r_ref.set(-1, ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(95, 190);
  r_ref.set(-1, ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(175, 190);
  r_ref.set(-1, ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(185, 190);
  r_ref.set(::cos(min), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-95, 190);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-45, 190);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 4th max(3)
  // min(0);
  COS(-350, -30);
  r_ref.set(-1, ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-310, -30);
  r_ref.set(-1, ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-40, -30);
  r_ref.set(::cos(min), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-370, -30);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-100, -30);
  r_ref.set(::cos(min), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-190, -30);
  r_ref.set(-1, ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
IntervalTest::acos()
{
  hkl::Interval r;
  hkl::Interval r_ref;

  r.set(-.5, .5);
  r.acos();
  r_ref.set(::acos(.5), ::acos(-.5));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
IntervalTest::sin(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[
  // 1 [pi/2, pi[
  // 2 [pi, 3*pi/2[
  // 3 [3*pi/2, 2*pi[
  hkl::Interval r;
  hkl::Interval r_ref;
  double min, max;

#define SIN(a,b)  do {\
    min = a *  HKL_DEGTORAD;\
    max = b *  HKL_DEGTORAD;\
    r.set(min, max);\
    r.sin();\
} while(0)

  // 1st max(0)
  // min(0);
  SIN(10, 14);
  r_ref.set(::sin(min), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-275, 14);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-15, 14);
  r_ref.set(::sin(min), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-95, 14);
  r_ref.set(-1, ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-185, 14);
  r_ref.set(-1, ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-215, 14);
  r_ref.set(-1, ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 2nd max(1)
  // min(0);
  SIN(10, 100);
  r_ref.set(::sin(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(85, 100);
  r_ref.set(::sin(max), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-20, 100);
  r_ref.set(::sin(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-110, 100);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-190, 100);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(95, 100);
  r_ref.set(::sin(max), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 3rd max(2)
  // min(0);
  SIN(10, 190);
  r_ref.set(::sin(max), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(95, 190);
  r_ref.set(::sin(max), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-95, 190);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(185, 190);
  r_ref.set(::sin(max), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-5, 190);
  r_ref.set(::sin(max), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-45, 190);
  r_ref.set(::sin(min), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 4th max(3)
  // min(0);
  SIN(-350, -30);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-40, -30);
  r_ref.set(::sin(min), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-370, -30);
  r_ref.set(-1, 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-100, -30);
  r_ref.set(-1, ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-170, -30);
  r_ref.set(-1, ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-190, -30);
  r_ref.set(-1, ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
IntervalTest::asin()
{
  hkl::Interval r;
  hkl::Interval r_ref;

  r.set(-.5, .5);
  r.asin();
  r_ref.set(::asin(-.5), ::asin(.5));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
IntervalTest::tan()
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[
  // 1 [pi/2, pi[
  // 2 [pi, 3*pi/2[
  // 3 [3*pi/2, 2*pi[
  hkl::Interval r;
  hkl::Interval r_ref;
  double min, max;

#define TAN(a,b)  do {\
    min = a *  HKL_DEGTORAD;\
    max = b *  HKL_DEGTORAD;\
    r.set(min, max);\
    r.tan();\
} while(0)

  TAN(-100, -89);
  r_ref.set(-INFINITY, INFINITY);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  /*
  // The limit case is not yet ok
  TAN(-100, 11, 12, -90);
  r_ref.set(-hkl::constant::math::infinity, ::tan(current), ::tan(consign), hkl::constant::math::infinity);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  */
}

void
IntervalTest::atan()
{
  hkl::Interval r;
  hkl::Interval r_ref;

  r.set(-10, 10);
  r.atan();
  r_ref.set(::atan(-10.), ::atan(10));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}
