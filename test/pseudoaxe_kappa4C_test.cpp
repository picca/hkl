#include "pseudoaxe_kappa4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa4C_Vertical_Test );

void
PseudoAxe_Kappa4C_Vertical_Test::setUp(void)
{
  m_geometry = hkl::kappa4C::vertical::Geometry();
  m_geometry_E4C = hkl::eulerian4C::vertical::Geometry();
}

void
PseudoAxe_Kappa4C_Vertical_Test::tearDown(void)
{}

void
PseudoAxe_Kappa4C_Vertical_Test::Omega(void)
{
  CPPUNIT_ASSERT_NO_THROW(m_geometry.get_source().setWaveLength(1.54));
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["omega"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(unsigned int i=-180;i<180;i++)
    {
      double angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Chi(void)
{
  int i;
  double angle;

  m_geometry.get_source().setWaveLength(1.54);
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["chi"];
  int chi_max = 100;

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  // but un-writable after un-initialization
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  //test exception if chi > 2*alpha
  angle = chi_max + 0.1;
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), hkl::HKLException);

  for(i=-chi_max;i<chi_max;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Phi(void)
{
  int i;
  double angle;

  m_geometry.get_source().setWaveLength(1.54);
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["phi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::kappa4C::vertical::pseudoAxeEngine::Psi pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  m_geometry_E4C.setAngles(45. * hkl::constant::math::degToRad,
                           77. * hkl::constant::math::degToRad,
                           -5. * hkl::constant::math::degToRad,
                           34. * hkl::constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), hkl::HKLException);

  // now initialize the the pseudoAxe.
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * hkl::constant::math::degToRad));

  // test the uninitialized state
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), hkl::HKLException);

  //set_current test1 non degenerate case
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * hkl::constant::math::degToRad));
  m_geometry_E4C.setFromGeometry(m_geometry, true);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45. * hkl::constant::math::degToRad), m_geometry_E4C.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77. * hkl::constant::math::degToRad), m_geometry_E4C.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5. * hkl::constant::math::degToRad), m_geometry_E4C.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), m_geometry_E4C.tth()->get_current());

  //set_current test2 degenerate case
  m_geometry_E4C.setAngles(30. * hkl::constant::math::degToRad,
                           0. * hkl::constant::math::degToRad,
                           0. * hkl::constant::math::degToRad,
                           60. * hkl::constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30. * hkl::constant::math::degToRad), m_geometry_E4C.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * hkl::constant::math::degToRad), m_geometry_E4C.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * hkl::constant::math::degToRad), m_geometry_E4C.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60. * hkl::constant::math::degToRad), m_geometry_E4C.tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  m_geometry.setAngles(1, 0, 0, 0);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  //get_value test
  m_geometry_E4C.setAngles(45. * hkl::constant::math::degToRad,
                           77. * hkl::constant::math::degToRad,
                           180. * hkl::constant::math::degToRad,
                           34. * hkl::constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      if ((i <= -174) || (i >= 47))
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
        }
    }

  m_geometry_E4C.setAngles(30. * hkl::constant::math::degToRad,
                           0. * hkl::constant::math::degToRad,
                           0. * hkl::constant::math::degToRad,
                           60. * hkl::constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      if (abs(i) > 100)
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Th2th(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  //set_current
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());

  // if put a non valid geometry can not set the value.
  m_geometry.setAngles(40. * hkl::constant::math::degToRad,
                       72. * hkl::constant::math::degToRad,
                       -1. * hkl::constant::math::degToRad,
                       30. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<10;i++)
    {
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          pseudoAxe.set_current(angle0);
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, hkl::constant::math::pi), fmod(angle, hkl::constant::math::pi), hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Q2th(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2;
  double value = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());

  // if put a non valid geometry can not get the value.
  m_geometry.setAngles(40. * hkl::constant::math::degToRad,
                       72. * hkl::constant::math::degToRad,
                       -1. * hkl::constant::math::degToRad,
                       30. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::Q(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       10 * hkl::constant::math::degToRad,
                       11 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.tth()->get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q = 0;
          CPPUNIT_ASSERT_NO_THROW(q = pseudoAxe.get_current().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa4C_Vertical_Test::persistanceIO(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians eulerians_ref(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians eulerians(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Psi psi_ref(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Psi psi(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Th2th th2th_ref(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Th2th th2th(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Q2th q2th_ref(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Q2th q2th(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Q q_ref(m_geometry);
  hkl::kappa4C::vertical::pseudoAxeEngine::Q q(m_geometry);
  stringstream flux;

  eulerians_ref.toStream(flux);
  psi_ref.toStream(flux);
  th2th_ref.toStream(flux);
  q2th_ref.toStream(flux);
  q_ref.toStream(flux);

  eulerians.fromStream(flux);
  psi.fromStream(flux);
  th2th.fromStream(flux);
  q2th.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(eulerians_ref, eulerians);
  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
  CPPUNIT_ASSERT_EQUAL(q2th_ref, q2th);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
