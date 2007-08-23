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
  CPPUNIT_ASSERT_THROW(Range range(2, 1, 0), HKLException);

  Range range(-1, 2, 3);
  CPPUNIT_ASSERT_EQUAL(Value(-1.), range.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(3.), range.get_max());

  // copy constructor
  Range range2(range);
  CPPUNIT_ASSERT_EQUAL(Value(-1.), range2.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2.), range2.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(3.), range2.get_max());
}

void
rangeTest::Equal()
{
  _range = Range(-1, 0, 1);
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
  CPPUNIT_ASSERT_NO_THROW(_range.set_current(Value(1.)));
  CPPUNIT_ASSERT_EQUAL( Value(1), _range.get_current());
}

void
rangeTest::persistanceIO(void)
{
  Range range_ref(Value(1), Value(2), Value(3));
  Range range1_ref(4, 5, 6);

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
