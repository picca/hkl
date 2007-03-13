#include "pseudoaxe_kappa6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa6C_Test );

void
PseudoAxe_Kappa6C_Test::setUp(void)
{
  m_geometry_E4C = geometry::eulerian4C::Vertical();
  m_geometry_E6C = geometry::Eulerian6C();
  m_geometry_K4C = geometry::kappa4C::Vertical();
  m_geometry = geometry::Kappa6C();
}

void
PseudoAxe_Kappa6C_Test::tearDown(void)
{}

void
PseudoAxe_Kappa6C_Test::Omega(void)
{
  int i;
  double angle;
  m_geometry.get_source().setWaveLength(1.54);
  hkl::pseudoAxeEngine::kappa6C::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["omega"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // random test
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
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
  hkl::pseudoAxeEngine::kappa6C::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["chi"];
  int chi_max = 100;

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  // but un-writable after un-initialization
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-m_geometry.get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(m_geometry.get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  //test exception if chi > 2*alpha
  angle = chi_max + 0.1;
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(angle), HKLException);

  for(i=-chi_max;i<chi_max;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
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
  hkl::pseudoAxeEngine::kappa6C::Eulerians pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["phi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0.), HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  for(i=-180;i<180;i++)
    {
      angle = i * constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon);
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
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
  hkl::pseudoAxeEngine::kappa6C::Psi pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                           77. * constant::math::degToRad,
                           -5. * constant::math::degToRad,
                           34. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), HKLException);

  // now initialize the the pseudoAxe.
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));

  // test the uninitialized state
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), HKLException);

  //set_current test1 non degenerate case
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  m_geometry_E4C.setFromGeometry(m_geometry, true);
  CPPUNIT_ASSERT_EQUAL(Value(45. * constant::math::degToRad), m_geometry_E4C.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77. * constant::math::degToRad), m_geometry_E4C.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5. * constant::math::degToRad), m_geometry_E4C.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), m_geometry_E4C.tth()->get_current());

  //set_current test2 degenerate case
  m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           60. * constant::math::degToRad);
  m_geometry.setFromGeometry(m_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(30. * constant::math::degToRad), m_geometry_E4C.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0. * constant::math::degToRad), m_geometry_E4C.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0. * constant::math::degToRad), m_geometry_E4C.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60. * constant::math::degToRad), m_geometry_E4C.tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  m_geometry.setAngles(0, 1, 0, 0, 0, 0);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Tth(void)
{
  hkl::pseudoAxeEngine::kappa6C::Tth pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["tth"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);


  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(+constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  //set_current
  m_geometry.setAngles(1. * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0. * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(pseudoAxe.get_current()));
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  pseudoAxe.set_current(36. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.delta()->get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      for(j=0;j<100;j++)
        {
          double angle0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle0));
          double angle = 0;
          CPPUNIT_ASSERT_NO_THROW(angle = pseudoAxe.get_current().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::Q(void)
{
  hkl::pseudoAxeEngine::kappa6C::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);

  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(0), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
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
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(1 * constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad), m_geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(77 * constant::math::degToRad), m_geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-5 * constant::math::degToRad), m_geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(0 * constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad), m_geometry.delta()->get_current());

  // peticular test
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0.));
  CPPUNIT_ASSERT_EQUAL(Value(0.), pseudoAxe.get_current());

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
      double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      pseudoAxe.initialize();
      for(j=0;j<100;j++)
        {
          double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
          double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
          pseudoAxe.set_current(q0);
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon);
        }
    }

  // test the set_write_from_read
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  Value read;
  Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(1.  * constant::math::degToRad,
                       45. * constant::math::degToRad,
                       77. * constant::math::degToRad,
                       -5. * constant::math::degToRad,
                       0.  * constant::math::degToRad,
                       34. * constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
}

void
PseudoAxe_Kappa6C_Test::persistanceIO(void)
{
  hkl::pseudoAxeEngine::kappa6C::Psi psi_ref(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Psi psi(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Eulerians eulerians_ref(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Eulerians eulerians(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Tth tth_ref(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Tth tth(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Q q_ref(m_geometry);
  hkl::pseudoAxeEngine::kappa6C::Q q(m_geometry);
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
