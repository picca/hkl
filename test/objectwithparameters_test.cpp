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
  vector<MyString> v(objectWithParameters.getParametersNames());
  
  CPPUNIT_ASSERT_EQUAL(MyString("p1"), v[0]);
  CPPUNIT_ASSERT_EQUAL(MyString("p2"), v[1]);

  objectWithParameters.setParameterValue("p1", 1.0);
  CPPUNIT_ASSERT_EQUAL(1.0, objectWithParameters.getParameterValue("p1"));
  objectWithParameters.setParameterValue("p2", 1.5);
  CPPUNIT_ASSERT_EQUAL(1.5, objectWithParameters.getParameterValue("p2"));
}


void
ObjectWithParametersTest::persistanceIO(void)
{
  ObjectWithParameters objectWithParameters_ref;
  ObjectWithParameters objectWithParameters;
  
  ObjectWithParameters objectWithParameters1_ref;
  ObjectWithParameters objectWithParameters1;  

  objectWithParameters_ref.addParameter("p1");
  objectWithParameters_ref.addParameter("p2");
 
  stringstream flux;
  objectWithParameters_ref.toStream(flux);
  objectWithParameters1_ref.toStream(flux);
  objectWithParameters.fromStream(flux);
  objectWithParameters1.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(objectWithParameters_ref, objectWithParameters);
  CPPUNIT_ASSERT_EQUAL(objectWithParameters1_ref, objectWithParameters1);
}
