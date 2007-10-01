#include "parameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ParameterTest );

void
ParameterTest::setUp(void) {}

void
ParameterTest::tearDown(void) {}

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
