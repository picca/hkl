#include "fitparameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( fitParameterTest );

void
fitParameterTest::setUp()
{
  m_fitParameter = FitParameter("toto", 0., -1. ,1., true, 1e-6);
}

void 
fitParameterTest::tearDown() 
{
}

void 
fitParameterTest::Constructor()
{
  FitParameter fitParameter("toto", 0., -1., 1., true, 1e-6);
  
  CPPUNIT_ASSERT_EQUAL(true, fitParameter.get_flagFit());
  CPPUNIT_ASSERT_EQUAL(1e-6, fitParameter.get_precision());
  CPPUNIT_ASSERT_EQUAL(0., fitParameter.get_chi2());
}

void 
fitParameterTest::Equal()
{
  FitParameter fitParameter("toto", 0., -1., 1., true, 1e-6);
  
  CPPUNIT_ASSERT_EQUAL(m_fitParameter, fitParameter);
}

void
fitParameterTest::CopyConstructor()
{
  FitParameter fitParameter(m_fitParameter);
  
  CPPUNIT_ASSERT_EQUAL(m_fitParameter, fitParameter);
}

void
fitParameterTest::GetSet()
{
  FitParameter fitParameter("toto", 0., -1., 1., true, 1e-6);
 
  fitParameter.set_flagFit(false);
  fitParameter.set_precision(1e-4);
  fitParameter.set_chi2(10.);

  CPPUNIT_ASSERT_EQUAL(false, fitParameter.get_flagFit());
  CPPUNIT_ASSERT_EQUAL(1e-4, fitParameter.get_precision());
  CPPUNIT_ASSERT_EQUAL(10., fitParameter.get_chi2());
}

void
fitParameterTest::persistanceIO(void)
{
  FitParameter fitParameter_ref("son nom", 1.345748912435689e-4, -1.45e3, 1.34e12,
                                true, 1e-6);
  
  FitParameter fitParameter;
  
  stringstream flux;
  fitParameter_ref.toStream(flux);
  fitParameter.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(fitParameter_ref, fitParameter);
}
