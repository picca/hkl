#include "pseudoaxe_kappa4C_test.h"
#include "constants.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa4C_Vertical_Test );

void
PseudoAxe_Kappa4C_Vertical_Test::setUp(void)
{
  m_geometry = geometry::kappa4C::Vertical();
  m_geometry_E4C = geometry::eulerian4C::Vertical();
}

void
PseudoAxe_Kappa4C_Vertical_Test::tearDown(void)
{}

void
PseudoAxe_Kappa4C_Vertical_Test::Omega(void)
{
  m_geometry.get_source().setWaveLength(1.54);
  hkl::pseudoAxe::kappa4C::vertical::Omega pseudoAxe(m_geometry);

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(unsigned int i=-180;i<180;i++)
    {
      double angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Chi(void)
{
  int i;
  double angle;

  m_geometry.get_source().setWaveLength(1.54);
  hkl::pseudoAxe::kappa4C::vertical::Chi pseudoAxe(m_geometry);
  int chi_max = 100;

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  // but un-writable after un-initialization
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  //test exception if chi > 2*alpha
  angle = chi_max + 0.1;
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), HKLException);

  for(i=-chi_max;i<chi_max;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Phi(void)
{
  int i;
  double angle;

  m_geometry.get_source().setWaveLength(1.54);
  hkl::pseudoAxe::kappa4C::vertical::Phi pseudoAxe(m_geometry);

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::pseudoAxe::kappa4C::vertical::eulerian4C::Psi pseudoAxe(m_geometry, "psi", "test");

  m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                           77. * constant::math::degToRad,
                           -5. * constant::math::degToRad,
                           34. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), HKLException);

  // now initialize the the pseudoAxe.
  m_geometry.setFromGeometry(m_geometry_E4C, true);
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
  m_geometry_E4C.setFromGeometry(m_geometry, true);
  CPPUNIT_ASSERT_EQUAL(Value(45. * constant::math::degToRad), m_geometry_E4C.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77. * constant::math::degToRad), m_geometry_E4C.get_axe("chi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5. * constant::math::degToRad), m_geometry_E4C.get_axe("phi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), m_geometry_E4C.get_axe("2theta").get_current());

  //set_current test2 degenerate case
  m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           60. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(30. * constant::math::degToRad),
                       m_geometry_E4C.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0. * constant::math::degToRad),
                       m_geometry_E4C.get_axe("chi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0. * constant::math::degToRad),
                       m_geometry_E4C.get_axe("phi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60. * constant::math::degToRad),
                       m_geometry_E4C.get_axe("2theta").get_current());

  // exception if the current geometry is not compatible with the initialization
  m_geometry.setAngles(1, 0, 0, 0);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());

  //get_value test
  m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                           77. * constant::math::degToRad,
                           180. * constant::math::degToRad,
                           34. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      if ((i <= -174) || (i >= 47))
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
        }
    }

  m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           60. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      if (abs(i) > 100)
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Th2th(void)
{
  hkl::pseudoAxe::kappa4C::vertical::twoC::Th2th pseudoAxe(m_geometry, "th2th", "test");

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  //set_current
  m_geometry.setAngles(45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // if put a non valid geometry can not set the value.
  m_geometry.setAngles(40. * constant::math::degToRad,
                       72. * constant::math::degToRad,
                       -1. * constant::math::degToRad,
                       30. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * constant::math::degToRad), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<10;i++)
    {
      double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          pseudoAxe.set_current(angle0);
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Q2th(void)
{
  hkl::pseudoAxe::kappa4C::vertical::twoC::Q2th pseudoAxe(m_geometry, "q2th", "test");

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

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
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // if put a non valid geometry can not get the value.
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
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
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
PseudoAxe_Kappa4C_Vertical_Test::Q(void)
{
  hkl::pseudoAxe::kappa4C::vertical::twoC::Q pseudoAxe(m_geometry, "q", "test");

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

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
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(10 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(11 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("komega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(10 * constant::math::degToRad),
                       m_geometry.get_axe("kappa").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(11 * constant::math::degToRad),
                       m_geometry.get_axe("kphi").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * constant::physic::tau * sin(theta) / lambda;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q;
          CPPUNIT_ASSERT_NO_THROW(q = pseudoAxe.get_current().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::persistanceIO(void)
{
  hkl::pseudoAxe::kappa4C::vertical::Omega omega_ref(m_geometry);
  hkl::pseudoAxe::kappa4C::vertical::Omega omega(m_geometry);
  stringstream flux;

  omega_ref.toStream(flux);

  omega.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
