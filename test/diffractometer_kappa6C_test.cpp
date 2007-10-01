#include "diffractometer_kappa6C_test.h"
#include "pseudoaxe.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa6CTest );

void
DiffractometerKappa6CTest::setUp(void) {}

void
DiffractometerKappa6CTest::tearDown(void) {}

/*
void
DiffractometerKappa6CTest::constructor(void)
{
  //hkl::kappa6C::Diffractometer diffractometer(10.);
  //CPPUNIT_ASSERT_EQUAL(10., diffractometer.getParameterhkl::Value("alpha"));
}

void
DiffractometerKappa6CTest::getSetAxes(void)
{
  // non existing axes
  CPPUNIT_ASSERT_THROW(m_diffractometer.setAxehkl::Value("pipo", 0.), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(m_diffractometer.getAxehkl::Value("pipo"), hkl::HKLException);

  // existing axes
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("mu"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("komega"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("kappa"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("kphi"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("gamma"));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.getAxeValue("delta"));

  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("mu", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("komega", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("kappa", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("kphi", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("gamma", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_diffractometer.setAxeValue("delta", 1.));

  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("mu"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("komega"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("kappa"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("kphi"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("gamma"));
  CPPUNIT_ASSERT_EQUAL(1., m_diffractometer.getAxeValue("delta"));
}
*/

void
DiffractometerKappa6CTest::pseudoAxes(void)
{
  hkl::kappa6C::Diffractometer * diffractometer = new hkl::kappa6C::Diffractometer(50 * HKL_DEGTORAD);

  hkl::PseudoAxe * omega = diffractometer->pseudoAxes()["omega"];
  hkl::PseudoAxe * chi = diffractometer->pseudoAxes()["chi"];
  hkl::PseudoAxe * phi = diffractometer->pseudoAxes()["phi"];

  //test the related pseudoAxes.
  hkl::Value omega_0 = omega->get_consign();
  hkl::Value phi_0 = phi->get_consign();
  unsigned int i;
  omega->parameters()["solution"]->set_current(1);
  for (i=0;i<100;i++)
    {
      double angle = i * HKL_DEGTORAD;
      chi->set_consign(angle);
      CPPUNIT_ASSERT_EQUAL(omega_0, omega->get_consign());
      CPPUNIT_ASSERT_EQUAL((hkl::Value)angle, chi->get_consign());
      CPPUNIT_ASSERT_EQUAL(phi_0, phi->get_consign());
    }

  omega->parameters()["solution"]->set_current(0);
  for (i=0;i<100;i++)
    {
      double angle = i * HKL_DEGTORAD;
      chi->set_consign(angle);
      CPPUNIT_ASSERT_EQUAL(omega_0, omega->get_consign());
      CPPUNIT_ASSERT_EQUAL((hkl::Value)angle, chi->get_consign());
      CPPUNIT_ASSERT_EQUAL(phi_0, phi->get_consign());
    }

  delete diffractometer;
}
