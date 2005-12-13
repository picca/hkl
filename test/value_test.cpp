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

void
valueTest::persistanceIO(void)
{
  Value value_ref("ca le fait grave\n cette valeur", 1.345748912435689e-4);
  value_ref.set_description("On en met une longue\navec un saut de ligne.");
  Value value1_ref("toto", 4);
  
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
