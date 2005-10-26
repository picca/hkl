#include "objectwithparameters_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ObjectWithParametersTest );

void
ObjectWithParametersTest::setUp(void)
{}

void 
ObjectWithParametersTest::tearDown(void) 
{}

void
ObjectWithParametersTest::equal(void)
{
  ObjectWithParameters objectWithParameters;

  CPPUNIT_ASSERT_EQUAL(objectWithParameters, objectWithParameters);
}

void
ObjectWithParametersTest::addParameter(void)
{
  ObjectWithParameters objectWithParameters;
  
  CPPUNIT_ASSERT_THROW(objectWithParameters.getParameterValue("p1"), HKLException);
  
  objectWithParameters.addParameter("p1");
  CPPUNIT_ASSERT_NO_THROW(objectWithParameters.getParameterValue("p1"));
  
  objectWithParameters.addParameter("p2");
  std::vector<std::string> v(objectWithParameters.getParametersNames());
  
  CPPUNIT_ASSERT_EQUAL(std::string("p1"), v[0]);
  CPPUNIT_ASSERT_EQUAL(std::string("p2"), v[1]);

  objectWithParameters.setParameterValue("p1", 1.0);
  CPPUNIT_ASSERT_EQUAL(1.0, objectWithParameters.getParameterValue("p1"));
  objectWithParameters.setParameterValue("p2", 1.5);
  CPPUNIT_ASSERT_EQUAL(1.5, objectWithParameters.getParameterValue("p2"));
}
