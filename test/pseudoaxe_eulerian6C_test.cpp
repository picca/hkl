#include "pseudoaxe_eulerian6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian6C_Vertical_Test );

void
PseudoAxe_Eulerian6C_Vertical_Test::setUp(void)
{
  m_geometry = hkl::eulerian6C::Geometry();
  m_geometry.setAngles(1. * hkl::constant::math::degToRad,
                       45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       0. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);

  _samples = new hkl::SampleList(m_geometry);
}

void
PseudoAxe_Eulerian6C_Vertical_Test::tearDown(void)
{
  delete _samples;
}

void
PseudoAxe_Eulerian6C_Vertical_Test::Tth(void)
{
  hkl::eulerian6C::pseudoAxeEngine::Tth pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["tth"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);


  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * hkl::constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(pseudoAxe.get_current()));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  pseudoAxe.set_current(36. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());

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

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
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
}

void
PseudoAxe_Eulerian6C_Vertical_Test::Q(void)
{
  hkl::eulerian6C::pseudoAxeEngine::Q pseudoAxeEngine(m_geometry);
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value(value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
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

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
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
}

void
PseudoAxe_Eulerian6C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::eulerian6C::pseudoAxeEngine::Psi pseudoAxeEngine(m_geometry, _samples);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1.), hkl::HKLException);

  // now initialize the the pseudoAxe.
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
  pseudoAxe.set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0 * hkl::constant::math::degToRad,
                          30 * hkl::constant::math::degToRad,
                          0 * hkl::constant::math::degToRad,
                          0 * hkl::constant::math::degToRad,
                          0 * hkl::constant::math::degToRad,
                          60 * hkl::constant::math::degToRad));
  m_geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30 * hkl::constant::math::degToRad), m_geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), m_geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60 * hkl::constant::math::degToRad), m_geometry.delta()->get_current());

  // test the writable change if the geometry is not compatible with the pseudoAxe initialization
  // in this case no effect.
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0, 0, 0, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  //get_current test
  m_geometry.setAngles(0 * hkl::constant::math::degToRad,
                       45 * hkl::constant::math::degToRad,
                       77 * hkl::constant::math::degToRad,
                       180 * hkl::constant::math::degToRad,
                       0 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  m_geometry.get_source().setWaveLength(1.54);

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

  // random test
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), hkl::constant::math::epsilon);
    }

  m_geometry.setAngles(0 * hkl::constant::math::degToRad,
                       30 * hkl::constant::math::degToRad,
                       0 * hkl::constant::math::degToRad,
                       0 * hkl::constant::math::degToRad,
                       0 * hkl::constant::math::degToRad,
                       60 * hkl::constant::math::degToRad);

  m_geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle,
                                   pseudoAxe.get_current().get_value(),
                                   hkl::constant::math::epsilon);
    }
}

void
PseudoAxe_Eulerian6C_Vertical_Test::persistanceIO(void)
{
  hkl::eulerian6C::pseudoAxeEngine::Tth tth_ref(m_geometry);
  hkl::eulerian6C::pseudoAxeEngine::Tth tth(m_geometry);
  hkl::eulerian6C::pseudoAxeEngine::Q q_ref(m_geometry);
  hkl::eulerian6C::pseudoAxeEngine::Q q(m_geometry);
  hkl::eulerian6C::pseudoAxeEngine::Psi psi_ref(m_geometry, _samples);
  hkl::eulerian6C::pseudoAxeEngine::Psi psi(m_geometry, _samples);
  stringstream flux;

  tth_ref.toStream(flux);
  q_ref.toStream(flux);
  psi_ref.toStream(flux);

  tth.fromStream(flux);
  q.fromStream(flux);
  psi.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(tth_ref, tth);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
}
