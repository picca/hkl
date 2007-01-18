#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{
  //m_geometry.get_source().setWaveLength(1.54);
  m_geometry = geometry::eulerian4C::Vertical();
  m_geometry.setAngles(45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       34. * constant::math::degToRad);
}

void
PseudoAxe_Eulerian4C_Vertical_Test::tearDown(void)
{}

void
PseudoAxe_Eulerian4C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::pseudoAxeEngine::eulerian4C::vertical::Psi pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

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
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
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
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.tth()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(30 * constant::math::degToRad,
                          0 * constant::math::degToRad,
                          0 * constant::math::degToRad,
                          60 * constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(30 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60 * constant::math::degToRad), m_geometry.tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(1, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());

  //random test1
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(45 * constant::math::degToRad,
                          77 * constant::math::degToRad,
                          180 * constant::math::degToRad,
                          34 * constant::math::degToRad));
  pseudoAxe.initialize();
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      // the pseudoAxe must be writable and readable
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
      CPPUNIT_ASSERT_EQUAL(Value(angle), pseudoAxe.get_current());
      // get_value change the writable state of the pseudoAxe.
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    }

  //random test2
  m_geometry.setAngles(30 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       0 * constant::math::degToRad,
                       60 * constant::math::degToRad);
  pseudoAxe.initialize();
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      pseudoAxe.set_current(angle);
      CPPUNIT_ASSERT_EQUAL(Value(angle),
                           pseudoAxe.get_current());
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Th2th(void)
{
  hkl::pseudoAxeEngine::eulerian4C::vertical::twoC::Th2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception after initialization.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1 * constant::math::degToRad));

  // test the uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // test the get and set after initialization
  m_geometry.setAngles(45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  //get_current
  m_geometry.setAngles(47. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       38. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(Value(38. * constant::math::degToRad), pseudoAxe.get_current());

  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.tth()->get_current());

  // put a non-compatible geometry and test the unactivation of the pseudoAxe.
  m_geometry.setAngles(0, 0, 0, 1);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  // random test
  /**
   * @todo angles0 initialisation must depend of omega0, chi0 etc...
   *  for now I am solving the problem by using Range::set_current(double) instead of Range::set_current(Value)
   *  in geometry conversion.
   */
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle0));
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi));
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q2th(void)
{
  hkl::pseudoAxeEngine::eulerian4C::vertical::twoC::Q2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

  // this pseudoAxe is always readable.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if not initialize
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2;
  double value = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
  m_geometry.setAngles(45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  pseudoAxe.set_current(value);
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.tth()->get_current());

  // if put a non valid geometry can not set the value.
  m_geometry.setAngles(40. * constant::math::degToRad,
                       72. * constant::math::degToRad,
                       -1. * constant::math::degToRad,
                       30. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * constant::math::degToRad), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // random test
  /**
   * @todo angles0 initialisation must depend of omega0, chi0 etc...
   * for now I am solving the problem by using Range::set_current(double) instead of Range::set_current(Value)
   * in geometry conversion.
   */
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * constant::physic::tau * sin(theta) / lambda;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q(void)
{
  hkl::pseudoAxeEngine::eulerian4C::vertical::twoC::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

  // this pseudoAxe is always redeable
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if not initialize.
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  m_geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  m_geometry.setAngles(45 * constant::math::degToRad,
                       10 * constant::math::degToRad,
                       11 * constant::math::degToRad,
                       34 * constant::math::degToRad);
  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * constant::math::degToRad;
  double value = 2 * constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(10 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(11 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value((double)value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(10 * constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(11 * constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.tth()->get_current());
  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * constant::physic::tau * sin(theta) / lambda;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::persistanceIO(void)
{
  hkl::pseudoAxeEngine::eulerian4C::vertical::Psi pseudoAxe_ref(m_geometry);
  hkl::pseudoAxeEngine::eulerian4C::vertical::Psi pseudoAxe(m_geometry);
  stringstream flux;

  pseudoAxe_ref.toStream(flux);

  pseudoAxe.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(pseudoAxe_ref, pseudoAxe);
}
