#include "constants.h"
#include "diffractometer_kappa6C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerKappa6CTest );

void
DiffractometerKappa6CTest::setUp(void)
{}

void
DiffractometerKappa6CTest::tearDown(void)
{}

/*
void
DiffractometerKappa6CTest::constructor(void)
{
  //diffractometer::Kappa6C diffractometer(10.);
  //CPPUNIT_ASSERT_EQUAL(10., diffractometer.getParameterValue("alpha"));
}
 
void
DiffractometerKappa6CTest::getSetAxes(void)
{
  // non existing axes
  CPPUNIT_ASSERT_THROW(m_diffractometer.setAxeValue("pipo", 0.), HKLException);
  CPPUNIT_ASSERT_THROW(m_diffractometer.getAxeValue("pipo"), HKLException);
 
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
  diffractometer::Kappa6C * diffractometer = new diffractometer::Kappa6C(50 * constant::math::degToRad);

  //Add reflections.
  Axe & delta = diffractometer->geometry()->get_axe("delta");
  Axe & komega = diffractometer->geometry()->get_axe("komega");
  Axe & kappa = diffractometer->geometry()->get_axe("kappa");
  Axe & kphi = diffractometer->geometry()->get_axe("kphi");

  PseudoMultiAxe * omega = diffractometer->_pseudoAxeEngine->pseudoAxes()[0];
  PseudoMultiAxe * chi = diffractometer->_pseudoAxeEngine->pseudoAxes()[1];
  PseudoMultiAxe * phi = diffractometer->_pseudoAxeEngine->pseudoAxes()[2];

  //test the related pseudoAxes.
  Value omega_0 = omega->get_current();
  Value phi_0 = phi->get_current();
  unsigned int i;
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
DiffractometerKappa6CTest::persistanceIO(void)
{
  diffractometer::Kappa6C d_ref(50 * constant::math::degToRad);
  diffractometer::Kappa6C d(50 * constant::math::degToRad);
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
  Axe & gamma = d_ref.geometry()->get_axe("gamma");
  Axe & delta = d_ref.geometry()->get_axe("delta");
  Axe & mu = d_ref.geometry()->get_axe("mu");
  Axe & komega = d_ref.geometry()->get_axe("komega");
  Axe & kappa = d_ref.geometry()->get_axe("kappa");
  Axe & kphi = d_ref.geometry()->get_axe("kphi");

  // Reflection 1
  gamma.set_current(30.398*constant::math::degToRad);
  delta.set_current(30.398*constant::math::degToRad);
  mu.set_current(11.709*constant::math::degToRad);
  komega.set_current(11.709*constant::math::degToRad);
  kappa.set_current(87.607*constant::math::degToRad);
  kphi.set_current(0.265*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(0., 0., 1.));

  // Reflection 2
  gamma.set_current(30.398*constant::math::degToRad);
  delta.set_current(21.001*constant::math::degToRad);
  mu.set_current(11.709*constant::math::degToRad);
  komega.set_current(10.322*constant::math::degToRad);
  kappa.set_current(-2.139*constant::math::degToRad);
  kphi.set_current(0.023*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(0., 2., 0.));

  // Reflection 3
  gamma.set_current(30.398*constant::math::degToRad);
  delta.set_current(54.046*constant::math::degToRad);
  mu.set_current(11.709*constant::math::degToRad);
  komega.set_current(26.872*constant::math::degToRad);
  kappa.set_current(34.938*constant::math::degToRad);
  kphi.set_current(57.295*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(-2., 2., 1.));

  // Reflection 4
  gamma.set_current(30.398*constant::math::degToRad);
  delta.set_current(37.333*constant::math::degToRad);
  mu.set_current(11.709*constant::math::degToRad);
  komega.set_current(18.51*constant::math::degToRad);
  kappa.set_current(53.966*constant::math::degToRad);
  kphi.set_current(54.505*constant::math::degToRad);
  d_ref.samples()->current()->reflections().add(svector(-1., 1., 1.));

  d_ref.toStream(flux);
  d.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
}
