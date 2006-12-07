#include "constants.h"
#include "diffractometer_kappa4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa4CTest );

void
DiffractometerKappa4CTest::setUp(void)
{
  m_diffractometer = diffractometer::kappa4C::Vertical();
}

void
DiffractometerKappa4CTest::tearDown(void)
{}

void
DiffractometerKappa4CTest::constructor(void)
{
  //CPPUNIT_ASSERT_EQUAL(10., diffractometer.getParameterValue("alpha"));
}

void
DiffractometerKappa4CTest::getSetAxes(void)
{
  // non existing axes
  CPPUNIT_ASSERT_THROW(m_diffractometer.setAxeValue("nu", 0.), HKLException);
  CPPUNIT_ASSERT_THROW(m_diffractometer.getAxeValue("nu"), HKLException);

  // existing axes
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("2theta"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("komega"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("kappa"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("kphi"));

  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("2theta", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("komega", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("kappa", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("kphi", 1.));

  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("2theta"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("komega"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("kappa"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("kphi"));
}
