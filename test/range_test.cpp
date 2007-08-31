#include "range_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( rangeTest );

void
rangeTest::setUp()
{
  _range = Range();
}

void
rangeTest::tearDown()
{}

void
rangeTest::Constructors()
{
  Value value;

  // default constructor
  CPPUNIT_ASSERT_EQUAL(value, _range.get_min());
  CPPUNIT_ASSERT_EQUAL(value, _range.get_current());
  CPPUNIT_ASSERT_EQUAL(value, _range.get_max());

  // 1st constructor
  CPPUNIT_ASSERT_THROW(Range range(2, 1, 1, 0), HKLException);

  Range range(-1, 2, 2, 3);
  CPPUNIT_ASSERT_EQUAL(Value(-1.), range.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range.get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(3.), range.get_max());

  // copy constructor
  Range range2(range);
  CPPUNIT_ASSERT_EQUAL(Value(-1.), range2.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range2.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range2.get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(3.), range2.get_max());
}

void
rangeTest::Equal()
{
  _range = Range(-1, 0, 0, 1);
  Range range(_range);

  CPPUNIT_ASSERT_EQUAL(_range, range);
}

void
rangeTest::GetSet()
{
  CPPUNIT_ASSERT_THROW(_range.set_range(Value(2.), Value(-1)), HKLException);
  CPPUNIT_ASSERT_THROW(_range.set_range(Value(-2.), Value(-1)), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_range.set_range(Value(-2.), Value(1)));
  CPPUNIT_ASSERT_EQUAL(Value(-2), _range.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(1), _range.get_max());

  CPPUNIT_ASSERT_THROW(_range.set_current(Value(-3.)), HKLException);
  CPPUNIT_ASSERT_THROW(_range.set_current(Value(3.)), HKLException);
  CPPUNIT_ASSERT_THROW(_range.set_consign(Value(-3.)), HKLException);
  CPPUNIT_ASSERT_THROW(_range.set_consign(Value(3.)), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_range.set_current(Value(1.)));
  CPPUNIT_ASSERT_NO_THROW(_range.set_consign(Value(1.)));
  CPPUNIT_ASSERT_EQUAL( Value(1), _range.get_current());
  CPPUNIT_ASSERT_EQUAL( Value(1), _range.get_consign());
}

void
rangeTest::operators(void)
{
  //operator+=(Range)
  hkl::Range r1(-1, 2, 3, 4);
  hkl::Range r2(-1, 2, 3, 4);
  hkl::Range r_ref(-2, 4, 6, 8);
  r1 += r2;
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator+=(double)
  r1 += 1;
  r_ref.set(-1, 5, 7, 9);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator*=(range)
  r1 *= r2;
  r_ref.set(-9, 10, 21, 36);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator*=(double)
  r1 *= -3;
  r_ref.set(-108, -30, -63, 27);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);

  // operator/=(double)
  r1 /= -3;
  r_ref.set(-9, 10, 21, 36);
  CPPUNIT_ASSERT_EQUAL(r_ref, r1);
}

void
rangeTest::cos(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[ 
  // 1 [pi/2, pi[ 
  // 2 [pi, 3*pi/2[ 
  // 3 [3*pi/2, 2*pi[
  hkl::Range r;
  hkl::Range r_ref;
  double min, current, consign , max;

#define COS(a,b,c,d)  do {\
    min = a *  hkl::constant::math::degToRad;\
    current = b *  hkl::constant::math::degToRad;\
    consign = c *  hkl::constant::math::degToRad;\
    max = d *  hkl::constant::math::degToRad;\
    r.set(min, current, consign, max);\
    r.cos();\
} while(0)

  // 1st max(0)
  // min(0);
  COS(10, 11, 12, 14);
  r_ref.set(::cos(max), ::cos(current), ::cos(consign), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-15, 11, 12, 14);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-95, 11, 12, 14);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-215, 11, 12, 14);
  r_ref.set(-1, ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 2nd max(1)
  // min(0);
  COS(10, 11, 12, 100);
  r_ref.set(::cos(max), ::cos(current), ::cos(consign), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-20, 11, 12, 100);
  r_ref.set(::cos(max), ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-110, 11, 12, 100);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-95, 11, 12, 100);
  r_ref.set(::cos(max), ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-190, 11, 12, 100);
  r_ref.set(-1, ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 3rd max(2)
  // min(0);
  COS(10, 11, 12, 190);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(95, 11, 12, 190);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(175, 11, 12, 190);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(185, 11, 12, 190);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-95, 11, 12, 190);
  r_ref.set(-1, ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-45, 11, 12, 190);
  r_ref.set(-1, ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 4th max(3)
  // min(0);
  COS(-350, 11, 12, -30);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-310, 11, 12, -30);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  COS(-40, 11, 12, -30);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  COS(-370, 11, 12, -30);
  r_ref.set(-1, ::cos(current), ::cos(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  COS(-100, 11, 12, -30);
  r_ref.set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  COS(-190, 11, 12, -30);
  r_ref.set(-1, ::cos(current), ::cos(consign), ::cos(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::acos()
{
  hkl::Range r;
  hkl::Range r_ref;

  r.set(-.5, 0, 0, .5);
  r.acos();
  r_ref.set(::acos(.5), ::acos(0), ::acos(0), ::acos(-.5));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::sin(void)
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[ 
  // 1 [pi/2, pi[ 
  // 2 [pi, 3*pi/2[ 
  // 3 [3*pi/2, 2*pi[
  hkl::Range r;
  hkl::Range r_ref;
  double min, current, consign , max;

#define SIN(a,b,c,d)  do {\
    min = a *  hkl::constant::math::degToRad;\
    current = b *  hkl::constant::math::degToRad;\
    consign = c *  hkl::constant::math::degToRad;\
    max = d *  hkl::constant::math::degToRad;\
    r.set(min, current, consign, max);\
    r.sin();\
} while(0)

  // 1st max(0)
  // min(0);
  SIN(10, 11, 12, 14);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-275, 11, 12, 14);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-15, 11, 12, 14);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-95, 11, 12, 14);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-185, 11, 12, 14);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-215, 11, 12, 14);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 2nd max(1)
  // min(0);
  SIN(10, 11, 12, 100);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(85, 11, 12, 100);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-20, 11, 12, 100);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-110, 11, 12, 100);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-190, 11, 12, 100);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(95, 11, 12, 100);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 3rd max(2)
  // min(0);
  SIN(10, 11, 12, 190);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(95, 11, 12, 190);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-95, 11, 12, 190);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(185, 11, 12, 190);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-5, 11, 12, 190);
  r_ref.set(::sin(max), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-45, 11, 12, 190);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // 4th max(3)
  // min(0);
  SIN(-350, 11, 12, -30);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(3);
  SIN(-40, 11, 12, -30);
  r_ref.set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-370, 11, 12, -30);
  r_ref.set(-1, ::sin(current), ::sin(consign), 1);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(2);
  SIN(-100, 11, 12, -30);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(max));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
  SIN(-170, 11, 12, -30);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);

  // min(1);
  SIN(-190, 11, 12, -30);
  r_ref.set(-1, ::sin(current), ::sin(consign), ::sin(min));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::asin()
{
  hkl::Range r;
  hkl::Range r_ref;

  r.set(-.5, 0, 0, .5);
  r.asin();
  r_ref.set(::asin(-.5), ::asin(0), ::asin(0), ::asin(.5));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::tan()
{
  // we will test all 16 cases.
  // we devide the trigonometric circle in 4 from quaters
  // 0 [0, pi/2[ 
  // 1 [pi/2, pi[ 
  // 2 [pi, 3*pi/2[ 
  // 3 [3*pi/2, 2*pi[
  hkl::Range r;
  hkl::Range r_ref;
  double min, current, consign , max;

#define TAN(a,b,c,d)  do {\
    min = a *  hkl::constant::math::degToRad;\
    current = b *  hkl::constant::math::degToRad;\
    consign = c *  hkl::constant::math::degToRad;\
    max = d *  hkl::constant::math::degToRad;\
    r.set(min, current, consign, max);\
    r.tan();\
} while(0)

  TAN(-100, 11, 12, -90);
  r_ref.set(-hkl::constant::math::infinity, ::tan(current), ::tan(consign), hkl::constant::math::infinity);
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::atan()
{
  hkl::Range r;
  hkl::Range r_ref;

  r.set(-10, 0, 0, 10);
  r.atan();
  r_ref.set(::atan(-10.), ::atan(0), ::atan(0), ::atan(10));
  CPPUNIT_ASSERT_EQUAL(r_ref, r);
}

void
rangeTest::persistanceIO(void)
{
  Range range_ref(Value(1), Value(2), Value(2), Value(3));
  Range range1_ref(4, 5, 5, 6);

  Range range;
  Range range1;

  std::stringstream flux;
  range_ref.toStream(flux);
  range1_ref.toStream(flux);
  range.fromStream(flux);
  range1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(range_ref, range);
  CPPUNIT_ASSERT_EQUAL(range1_ref, range1);
}
