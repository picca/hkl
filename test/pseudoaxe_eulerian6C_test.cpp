#include "pseudoaxe_eulerian6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian6C_Vertical_Test );

void
PseudoAxe_Eulerian6C_Vertical_Test::setUp(void)
{
  m_geometry = geometry::Eulerian6C();
  m_geometry.setAngles(1. * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0. * constant::math::degToRad,
                       34. * constant::math::degToRad);
}

void
PseudoAxe_Eulerian6C_Vertical_Test::tearDown(void)
{}

void
PseudoAxe_Eulerian6C_Vertical_Test::Tth(void)
{
  hkl::pseudoAxe::eulerian6C::Tth pseudoAxe(m_geometry);

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);


  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(pseudoAxe.get_current()));
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  pseudoAxe.set_current(36. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.delta()->get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      for(j=0;j<100;j++)
        {
          double angle0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle0));
          double angle;
          CPPUNIT_ASSERT_NO_THROW(angle = pseudoAxe.get_current().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Eulerian6C_Vertical_Test::Q(void)
{
  hkl::pseudoAxe::eulerian6C::Q pseudoAxe(m_geometry);

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  //set_current
  m_geometry.setAngles(1. * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0. * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * constant::math::degToRad;
  double value = 2 * constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(Value(value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.delta()->get_current());

  // peticular test
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  CPPUNIT_ASSERT_EQUAL(Value(0.), pseudoAxe.get_current());

  // test the writable change if the geometry is not compatible with the pseudoAxe initialization
  // in this case no effect.
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  m_geometry.setAngles(0, 0, 0, 0, 0, 0);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
      pseudoAxe.initialize();
      for(j=0;j<100;j++)
        {
          double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
          double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
          pseudoAxe.set_current(q0);
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Eulerian6C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::pseudoAxe::eulerian6C::eulerian4C::vertical::Psi pseudoAxe(m_geometry, "psi", "test");

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), HKLException);

  // now initialize the the pseudoAxe.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));

  // test the uninitialized state
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), HKLException);

  //set_current test1 non degenerate case
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  pseudoAxe.set_current(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.delta()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0 * constant::math::degToRad,
                          30 * constant::math::degToRad,
                          0 * constant::math::degToRad,
                          0 * constant::math::degToRad,
                          0 * constant::math::degToRad,
                          60 * constant::math::degToRad));
  m_geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60 * constant::math::degToRad), m_geometry.delta()->get_current());

  // test the writable change if the geometry is not compatible with the pseudoAxe initialization
  // in this case no effect.
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0, 0, 0, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());

  //get_current test
  m_geometry.setAngles(0 * constant::math::degToRad,
                       45 * constant::math::degToRad,
                       77 * constant::math::degToRad,
                       180 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       34 * constant::math::degToRad);
  m_geometry.get_source().setWaveLength(1.54);

  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
    }

  m_geometry.setAngles(0 * constant::math::degToRad,
                       30 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       60 * constant::math::degToRad);

  m_geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle,
                                   pseudoAxe.get_current().get_value(),
                                   constant::math::epsilon_0);
    }
}

void
PseudoAxe_Eulerian6C_Vertical_Test::persistanceIO(void)
{
  hkl::pseudoAxe::eulerian6C::eulerian4C::vertical::Psi psi_ref(m_geometry, "psi_ref", "test");
  hkl::pseudoAxe::eulerian6C::eulerian4C::vertical::Psi psi(m_geometry, "psi", "test");
  stringstream flux;

  psi_ref.toStream(flux);

  psi.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
}
