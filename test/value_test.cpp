#include "value_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( valueTest );

void
valueTest::setUp()
{
  m_value = Value("value", 4.);
}

void 
valueTest::tearDown() 
{
}

void 
valueTest::Constructor()
{
  Value value("value", 4.);
  
  CPPUNIT_ASSERT_EQUAL(4., value.get_value());
}

void 
valueTest::Equal()
{
  Value value("value", 4.);
  
  CPPUNIT_ASSERT_EQUAL(m_value, value);
}

void
valueTest::CopyConstructor()
{
  Value value(m_value);
  
  CPPUNIT_ASSERT_EQUAL(m_value, value);
}

void
valueTest::GetSet()
{
  Value value("value", 4.);
  
  value.set_value(5.);
  CPPUNIT_ASSERT_EQUAL(5., value.get_value());
}

void
valueTest::PlusEqual(void)
{
  Value value("value", 4.);
  Value value2("value", 3.);

  value += value2;
  CPPUNIT_ASSERT_EQUAL(7., value.get_value());

  value += value;
  CPPUNIT_ASSERT_EQUAL(14., value.get_value());
}

void
valueTest::DivideEqual(void)
{
  Value value("value", 4.);
  Value value2("value", 3.);

  value /= 2.;
  CPPUNIT_ASSERT_EQUAL(2., value.get_value());
}

