#include "diffractometer_kappa4C_test.h"
#include "pseudoaxe.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa4CTest );

void
DiffractometerKappa4CTest::setUp(void)
{}

void
DiffractometerKappa4CTest::tearDown(void)
{}

void
DiffractometerKappa4CTest::pseudoAxes(void)
{
  hkl::kappa4C::vertical::Diffractometer * diffractometer = new hkl::kappa4C::vertical::Diffractometer(50 * hkl::constant::math::degToRad);

  hkl::PseudoAxe * omega = diffractometer->pseudoAxes()["omega"];
  hkl::PseudoAxe * chi = diffractometer->pseudoAxes()["chi"];
  hkl::PseudoAxe * phi = diffractometer->pseudoAxes()["phi"];

  //test the related pseudoAxes solution 1 and 0.
  hkl::Value omega_0 = omega->get_current();
  hkl::Value phi_0 = phi->get_current();
  unsigned int i;
  omega->parameters()["solution"]->set_current(1);
  for(i=0;i<100;i++)
    {
      double angle = i * hkl::constant::math::degToRad;
      chi->set_current(angle);
      CPPUNIT_ASSERT_EQUAL(omega_0, omega->get_current());
      CPPUNIT_ASSERT_EQUAL((hkl::Value)angle, chi->get_current());
      CPPUNIT_ASSERT_EQUAL(phi_0, phi->get_current());
    }

  omega->parameters()["solution"]->set_current(0);
  for(i=0;i<100;i++)
    {
      double angle = i * hkl::constant::math::degToRad;
      chi->set_current(angle);
      CPPUNIT_ASSERT_EQUAL(omega_0, omega->get_current());
      CPPUNIT_ASSERT_EQUAL((hkl::Value)angle, chi->get_current());
      CPPUNIT_ASSERT_EQUAL(phi_0, phi->get_current());
    }

  delete diffractometer;
}

void
DiffractometerKappa4CTest::persistanceIO(void)
{
  hkl::kappa4C::vertical::Diffractometer d_ref(50 * hkl::constant::math::degToRad);
  hkl::kappa4C::vertical::Diffractometer d(50 * hkl::constant::math::degToRad);
  stringstream flux;

  d_ref.geometry()->get_source().setWaveLength(2.43);
  d_ref.samples().add("titi", hkl::SAMPLE_MONOCRYSTAL);
  d_ref.samples().set_current(0);
  hkl::Lattice & lattice = d_ref.samples().current()->lattice();
  lattice.a().set_current(2.34);
  lattice.b().set_current(4.5);
  lattice.c().set_current(2.7);
  lattice.alpha().set_current(90 * hkl::constant::math::degToRad);
  lattice.beta().set_current(120 * hkl::constant::math::degToRad);
  lattice.gamma().set_current(60 * hkl::constant::math::degToRad);
  //d_ref.modes().set_current("Symetric");

  //Add reflections.
  hkl::Axe & tth = d_ref.geometry()->get_axe("2theta");
  hkl::Axe & komega = d_ref.geometry()->get_axe("komega");
  hkl::Axe & kappa = d_ref.geometry()->get_axe("kappa");
  hkl::Axe & kphi = d_ref.geometry()->get_axe("kphi");

  // Reflection 1
  tth.set_current(30.398*hkl::constant::math::degToRad);
  komega.set_current(11.709*hkl::constant::math::degToRad);
  kappa.set_current(87.607*hkl::constant::math::degToRad);
  kphi.set_current(0.265*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 0., 1.));

  // Reflection 2
  tth.set_current(21.001*hkl::constant::math::degToRad);
  komega.set_current(10.322*hkl::constant::math::degToRad);
  kappa.set_current(-2.139*hkl::constant::math::degToRad);
  kphi.set_current(0.023*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 2., 0.));

  // Reflection 3
  tth.set_current(54.046*hkl::constant::math::degToRad);
  komega.set_current(26.872*hkl::constant::math::degToRad);
  kappa.set_current(34.938*hkl::constant::math::degToRad);
  kphi.set_current(57.295*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-2., 2., 1.));

  // Reflection 4
  tth.set_current(37.333*hkl::constant::math::degToRad);
  komega.set_current(18.51*hkl::constant::math::degToRad);
  kappa.set_current(53.966*hkl::constant::math::degToRad);
  kphi.set_current(54.505*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-1., 1., 1.));

  d_ref.toStream(flux);
  d.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
}
