#include "fitparameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( FitParameterTest );

void
FitParameterTest::setUp(void)
{}

void 
FitParameterTest::tearDown(void) 
{}

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

void
FitParameterTest::persistanceIO(void)
{
    FitParameter fitParameter_ref("son nom", "sa description", 
                                  -1.345748912435689e-4, 1.45e3, 1.34e12,
                                  true, 1e-6);

    FitParameter fitParameter1_ref("son nom", "l", 
                                  -1.345748912435689e-4, 1.45e3, 1.34e12,
                                  true, 1e-6);

    FitParameter fitParameter("nul", "de chez nul",
                              -1, 2, 3,
                              false, 0.);

    FitParameter fitParameter1("nul", "de chez nul",
                              -1, 2, 3,
                              false, 0.);

    stringstream flux;
    fitParameter_ref.toStream(flux);
    fitParameter1_ref.toStream(flux);
    fitParameter.fromStream(flux);
    fitParameter1.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(fitParameter_ref, fitParameter);
    CPPUNIT_ASSERT_EQUAL(fitParameter1_ref, fitParameter1);
}
