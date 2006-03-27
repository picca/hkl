#include "constants.h"
#include "diffractometer_kappa6C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa6CTest );

void
DiffractometerKappa6CTest::setUp(void)
{
    double alpha = 50. * constant::math::degToRad;
    m_diffractometer = new diffractometer::Kappa6C(alpha);
}

void 
DiffractometerKappa6CTest::tearDown(void)
{
    delete m_diffractometer;
}

void
DiffractometerKappa6CTest::constructor(void)
{
    diffractometer::Kappa6C diffractometer(10.);
    CPPUNIT_ASSERT_EQUAL(10., diffractometer.getParameterValue("alpha"));
}

void
DiffractometerKappa6CTest::getSetAxes(void)
{
    // non existing axes
    CPPUNIT_ASSERT_THROW(m_diffractometer->setAxeValue("pipo", 0.), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->getAxeValue("pipo"), HKLException);

    // existing axes
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("mu"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("komega"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("kappa"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("kphi"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("gamma"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("delta"));

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("mu", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("komega", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("kappa", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("kphi", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("gamma", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("delta", 1.));

    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("mu"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("komega"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("kappa"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("kphi"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("gamma"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("delta"));
}
