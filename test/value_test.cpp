#include "value_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( valueTest );

void
valueTest::setUp()
{
  m_value = Value();
}

void 
valueTest::tearDown() 
{
}

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

  value.set_value(5. + hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_EQUAL(m_value, value);

  value.set_value(5. + hkl::constant::math::epsilon2);
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
valueTest::persistanceIO(void)
{
  Value value_ref(3);
  Value value1_ref(4);
  
  Value value;
  Value value1;
  
  stringstream flux;
  value_ref.toStream(flux);
  value1_ref.toStream(flux);
  value.fromStream(flux);
  value1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(value_ref, value);
  CPPUNIT_ASSERT_EQUAL(value1_ref, value1);
}
