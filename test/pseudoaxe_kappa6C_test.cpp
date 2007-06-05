#include "pseudoaxe_kappa6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa6C_Test );

void
PseudoAxe_Kappa6C_Test::setUp(void)
{
  m_geometry_E4C = hkl::eulerian4C::vertical::Geometry();
  m_geometry_E6C = hkl::eulerian6C::Geometry();
  m_geometry_K4C = hkl::kappa4C::vertical::Geometry();
  m_geometry = hkl::kappa6C::Geometry();

  _samples = new hkl::SampleList(m_geometry);
}

void
PseudoAxe_Kappa6C_Test::tearDown(void)
{
  delete _samples;
}

void
PseudoAxe_Kappa6C_Test::Omega(void)
{
  int i;
  double angle;
  m_geometry.get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["omega"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // random test
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Chi(void)
{
  int i;
  double angle;
  m_geometry.get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Phi(void)
{
  int i;
  double angle;
  m_geometry.get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["phi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::kappa6C::pseudoAxeEngine::Psi pseudoAxeEngine(m_geometry, _samples);
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
  m_geometry.setAngles(0, 1, 0, 0, 0, 0);
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Tth(void)
{
  hkl::kappa6C::pseudoAxeEngine::Tth pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["tth"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);


  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * hkl::constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  //set_current
  m_geometry.setAngles(1. * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(pseudoAxe.get_current()));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  pseudoAxe.set_current(36. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      for(j=0;j<100;j++)
        {
          double angle0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle0));
          double angle = 0;
          CPPUNIT_ASSERT_NO_THROW(angle = pseudoAxe.get_current().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, hkl::constant::math::pi), fmod(angle, hkl::constant::math::pi), hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Q(void)
{
  hkl::kappa6C::pseudoAxeEngine::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * hkl::constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  //set_current
  m_geometry.setAngles(1. * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());

  // peticular test
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), pseudoAxe.get_current());

  // test the writable change if the geometry is not compatible with the pseudoAxe initialization
  // in this case no effect.
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  m_geometry.setAngles(0, 0, 0, 0, 0, 0);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      pseudoAxe.initialize();
      for(j=0;j<100;j++)
        {
          double theta = hkl::constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
          double q0 = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
          pseudoAxe.set_current(q0);
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
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
  m_geometry.setAngles(1.  * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0.  * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::persistanceIO(void)
{
  hkl::kappa6C::pseudoAxeEngine::Psi psi_ref(m_geometry, _samples);
  hkl::kappa6C::pseudoAxeEngine::Psi psi(m_geometry, _samples);
  hkl::kappa6C::pseudoAxeEngine::Eulerians eulerians_ref(m_geometry);
  hkl::kappa6C::pseudoAxeEngine::Eulerians eulerians(m_geometry);
  hkl::kappa6C::pseudoAxeEngine::Tth tth_ref(m_geometry);
  hkl::kappa6C::pseudoAxeEngine::Tth tth(m_geometry);
  hkl::kappa6C::pseudoAxeEngine::Q q_ref(m_geometry);
  hkl::kappa6C::pseudoAxeEngine::Q q(m_geometry);
  stringstream flux;

  psi_ref.toStream(flux);
  eulerians_ref.toStream(flux);
  tth_ref.toStream(flux);
  q_ref.toStream(flux);

  psi.fromStream(flux);
  eulerians.fromStream(flux);
  tth.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
  CPPUNIT_ASSERT_EQUAL(eulerians_ref, eulerians);
  CPPUNIT_ASSERT_EQUAL(tth_ref, tth);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
