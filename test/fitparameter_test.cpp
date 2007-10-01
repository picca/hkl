#include "fitparameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( FitParameterTest );

void
FitParameterTest::setUp(void) {}

void
FitParameterTest::tearDown(void) {}

void
FitParameterTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(FitParameter("", "", 2, 1, 3, true, 1e-6), HKLException);
  CPPUNIT_ASSERT_THROW(FitParameter("toto", "", 2, 1, 3, true, 1e-6), HKLException);
  CPPUNIT_ASSERT_THROW(FitParameter("toto", "titi", 2, 1, 3, true, 1e-6), HKLException);
  CPPUNIT_ASSERT_NO_THROW(FitParameter("toto", "titi", 1, 2, 3, true, 1e-6));

  // 1st constructor
  FitParameter fitParameter("toto", "titi", -1, 0., 1., true, 1e-6);
  CPPUNIT_ASSERT_EQUAL(true, fitParameter.get_flagFit());
  CPPUNIT_ASSERT_EQUAL(Value(1e-6), fitParameter.get_precision());
  CPPUNIT_ASSERT_EQUAL(Value(0.), fitParameter.get_chi2());

  // copy constructor
  FitParameter fitParameter1(fitParameter);
  CPPUNIT_ASSERT_EQUAL(fitParameter, fitParameter1);
}

void
FitParameterTest::getSet(void)
{
  FitParameter fitParameter("toto", "tutu", -1, 0., 1., true, 1e-6);

  fitParameter.set_flagFit(false);
  fitParameter.set_precision(1e-4);
  fitParameter.set_chi2(10.);

  CPPUNIT_ASSERT_EQUAL(false, fitParameter.get_flagFit());
  CPPUNIT_ASSERT_EQUAL(Value(1e-4), fitParameter.get_precision());
  CPPUNIT_ASSERT_EQUAL(Value(10.), fitParameter.get_chi2());
}
