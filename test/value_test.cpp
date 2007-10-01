#include "config.h"
#include "value_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( valueTest );

void
valueTest::setUp()
{
  m_value = Value();
}

void
valueTest::tearDown() {}

void
valueTest::Constructors()
{
  //default
  CPPUNIT_ASSERT_EQUAL(0., m_value.get_value());

  // 2nd constructor
  Value value(2.);
  CPPUNIT_ASSERT_EQUAL(2., value.get_value());

  //copy constructor
  Value value2(value);
  CPPUNIT_ASSERT_EQUAL(2., value2.get_value());
}

void
valueTest::GetSet()
{
  m_value.set_value(5.);

  CPPUNIT_ASSERT_EQUAL(5., m_value.get_value());
}

void
valueTest::Comparisons()
{
  m_value.set_value(5.);
  Value value(m_value);

  //m_value == value
  CPPUNIT_ASSERT_EQUAL(m_value, value);

  value.set_value(5. + HKL_EPSILON / 2);
  CPPUNIT_ASSERT_EQUAL(m_value, value);

  value.set_value(5. + HKL_EPSILON);
  CPPUNIT_ASSERT( !(m_value == value) );

  //m_value <= value
  CPPUNIT_ASSERT(m_value <= value);
  value.set_value(6.);
  CPPUNIT_ASSERT(m_value <= value);
  CPPUNIT_ASSERT( !(value <= m_value) );
}

void
valueTest::PlusEqual(void)
{
  m_value.set_value(4.);
  Value value;
  value.set_value(3.);

  value += m_value;
  CPPUNIT_ASSERT_EQUAL(7., value.get_value());

  value += value;
  CPPUNIT_ASSERT_EQUAL(14., value.get_value());
}

void
valueTest::DivideEqual(void)
{
  Value value(4.);

  value /= Value(2.);
  CPPUNIT_ASSERT_EQUAL(Value(2.), value);
}

void
valueTest::operators(void)
{
  Value value1(2.);
  Value value2(2.);

  // operator +
  CPPUNIT_ASSERT_EQUAL(Value(4.), value1 + value2);
  // operator -
  CPPUNIT_ASSERT_EQUAL(Value(0.), value1 - value2);
  // operator *
  CPPUNIT_ASSERT_EQUAL(Value(4.), value1 * value2);
  // operator /
  CPPUNIT_ASSERT_EQUAL(Value(1.), value1 / value2);
}

void
valueTest::fabs(void)
{
  Value value(-4.);

  CPPUNIT_ASSERT_EQUAL(Value(4), ::fabs(value));
}
