#include "diffractometer_kappa4C_test.h"
#include "pseudoaxe.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa4CTest );

void
DiffractometerKappa4CTest::setUp(void) {}

void
DiffractometerKappa4CTest::tearDown(void) {}

void
DiffractometerKappa4CTest::pseudoAxes(void)
{
  hkl::kappa4C::vertical::Diffractometer * diffractometer = new hkl::kappa4C::vertical::Diffractometer(50 * HKL_DEGTORAD);

  hkl::PseudoAxe * omega = diffractometer->pseudoAxes()["omega"];
  hkl::PseudoAxe * chi = diffractometer->pseudoAxes()["chi"];
  hkl::PseudoAxe * phi = diffractometer->pseudoAxes()["phi"];

  //test the related pseudoAxes solution 1 and 0.
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
