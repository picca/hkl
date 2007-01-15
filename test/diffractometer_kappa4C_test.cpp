#include "constants.h"
#include "diffractometer_kappa4C_test.h"

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
  diffractometer::kappa4C::Vertical * diffractometer = new diffractometer::kappa4C::Vertical(50 * constant::math::degToRad);

  //Add reflections.
  Axe & tth = diffractometer->geometry()->get_axe("2theta");
  Axe & komega = diffractometer->geometry()->get_axe("komega");
  Axe & kappa = diffractometer->geometry()->get_axe("kappa");
  Axe & kphi = diffractometer->geometry()->get_axe("kphi");

  PseudoMultiAxe * omega = diffractometer->_pseudoAxeEngine->pseudoAxes()[0];
  PseudoMultiAxe * chi = diffractometer->_pseudoAxeEngine->pseudoAxes()[1];
  PseudoMultiAxe * phi = diffractometer->_pseudoAxeEngine->pseudoAxes()[2];

  //test the related pseudoAxes solution 1 and 0.
  Value omega_0 = omega->get_current();
  Value phi_0 = phi->get_current();
  unsigned int i;
  diffractometer->_pseudoAxeEngine->parameters()["solution"]->set_current(1);
  for(i=0;i<100;i++)
    {
      double angle = i * constant::math::degToRad;
      cout << "i: " << i << endl;
      cout << omega->get_current().get_value() << " " << angle << " " << phi->get_current().get_value() << endl;
      chi->set_current(angle);
      CPPUNIT_ASSERT_EQUAL(Value(omega_0), omega->get_current());
      CPPUNIT_ASSERT_EQUAL(Value(angle), chi->get_current());
      CPPUNIT_ASSERT_EQUAL(Value(phi_0), phi->get_current());
    }

  diffractometer->_pseudoAxeEngine->parameters()["solution"]->set_current(0);
  for(i=0;i<100;i++)
    {
      double angle = i * constant::math::degToRad;
      cout << "i: " << i << endl;
      cout << omega->get_current().get_value() << " " << angle << " " << phi->get_current().get_value() << endl;
      chi->set_current(angle);
      CPPUNIT_ASSERT_EQUAL(Value(omega_0), omega->get_current());
      CPPUNIT_ASSERT_EQUAL(Value(angle), chi->get_current());
      CPPUNIT_ASSERT_EQUAL(Value(phi_0), phi->get_current());
    }

  delete diffractometer;
}

void
DiffractometerKappa4CTest::persistanceIO(void)
{
  diffractometer::kappa4C::Vertical d_ref(50 * constant::math::degToRad);
  diffractometer::kappa4C::Vertical d(50 * constant::math::degToRad);
  stringstream flux;

  d_ref.geometry()->get_source().setWaveLength(2.43);
  d_ref.samples()->add("titi", SAMPLE_MONOCRYSTAL);
  d_ref.samples()->set_current(0);
  hkl::Lattice & lattice = d_ref.samples()->current()->lattice();
  lattice.a().set_current(2.34);
  lattice.b().set_current(4.5);
  lattice.c().set_current(2.7);
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(120 * constant::math::degToRad);
  lattice.gamma().set_current(60 * constant::math::degToRad);
  //d_ref.modes().set_current("Symetric");

  //Add reflections.
  Axe & tth = d_ref.geometry()->get_axe("2theta");
  Axe & komega = d_ref.geometry()->get_axe("komega");
  Axe & kappa = d_ref.geometry()->get_axe("kappa");
  Axe & kphi = d_ref.geometry()->get_axe("kphi");

  // Reflection 1
  tth.set_current(30.398*constant::math::degToRad);
  komega.set_current(11.709*constant::math::degToRad);
  kappa.set_current(87.607*constant::math::degToRad);
  kphi.set_current(0.265*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(0., 0., 1.));

  // Reflection 2
  tth.set_current(21.001*constant::math::degToRad);
  komega.set_current(10.322*constant::math::degToRad);
  kappa.set_current(-2.139*constant::math::degToRad);
  kphi.set_current(0.023*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(0., 2., 0.));

  // Reflection 3
  tth.set_current(54.046*constant::math::degToRad);
  komega.set_current(26.872*constant::math::degToRad);
  kappa.set_current(34.938*constant::math::degToRad);
  kphi.set_current(57.295*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(-2., 2., 1.));

  // Reflection 4
  tth.set_current(37.333*constant::math::degToRad);
  komega.set_current(18.51*constant::math::degToRad);
  kappa.set_current(53.966*constant::math::degToRad);
  kphi.set_current(54.505*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(-1., 1., 1.));

  d_ref.toStream(flux);
  d.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
}
