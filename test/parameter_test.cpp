#include "parameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ParameterTest );

void
ParameterTest::setUp(void)
{}

void
ParameterTest::tearDown(void)
{}

void
ParameterTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(Parameter("", "", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_THROW(Parameter("", "coucou", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_THROW(Parameter("toto", "coucou", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_NO_THROW(Parameter("toto", "coucou", 1, 2, 3));

  Parameter parameter_ref("toto", "coucou", 1, 2, 3);
  Parameter parameter(parameter_ref);
  CPPUNIT_ASSERT_EQUAL(parameter_ref, parameter);
}

void
ParameterTest::persistanceIO(void)
{
  Parameter parameter_ref("ca le fait grave", "de la balle je vous le dit\ncoucou", -7.432165432, 1.34e-32, 8.);
  Parameter parameter("titi", "tutu", 1., 2, 3);
  Parameter parameter1_ref("another object", "with a nice description", 2, 3, 4);
  Parameter parameter1(parameter);

  stringstream flux;
  parameter_ref.toStream(flux);
  parameter1_ref.toStream(flux);
  parameter.fromStream(flux);
  parameter1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(parameter_ref, parameter);
  CPPUNIT_ASSERT_EQUAL(parameter1_ref, parameter1);
}
