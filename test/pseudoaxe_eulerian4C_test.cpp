#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{
  //_geometry->get_source().setWaveLength(1.54);
  _geometry = new hkl::eulerian4C::vertical::Geometry;
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);

  _samples = new hkl::SampleList(*_geometry);
}

void
PseudoAxe_Eulerian4C_Vertical_Test::tearDown(void)
{
  delete _samples;
  delete _geometry;
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi pseudoAxeEngine(*_geometry, _samples);
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(_geometry->setAngles(30 * hkl::constant::math::degToRad,
                          0 * hkl::constant::math::degToRad,
                          0 * hkl::constant::math::degToRad,
                          60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  CPPUNIT_ASSERT_NO_THROW(_geometry->setAngles(1, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  // test the set_write_from_read
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
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
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

  //random test1
  CPPUNIT_ASSERT_NO_THROW(_geometry->setAngles(45 * hkl::constant::math::degToRad,
                                               77 * hkl::constant::math::degToRad,
                                               180 * hkl::constant::math::degToRad,
                                               34 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      // the pseudoAxe must be writable and readable
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
      // get_value change the writable state of the pseudoAxe.
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
    }

  //random test2
  CPPUNIT_ASSERT_NO_THROW(_geometry->setAngles(30 * hkl::constant::math::degToRad,
                                               0 * hkl::constant::math::degToRad,
                                               0 * hkl::constant::math::degToRad,
                                               60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for(i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle));
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Th2th(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // no more exception after initialization.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1 * hkl::constant::math::degToRad));

  // test the uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);

  // test the get and set after initialization
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  //get_current
  _geometry->setAngles(47. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       38. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(38. * hkl::constant::math::degToRad), pseudoAxe.get_current());

  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  // put a non-compatible geometry and test the unactivation of the pseudoAxe.
  _geometry->setAngles(0, 0, 0, 1);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  // test the set_write_from_read
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
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
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

  // random test
  /**
   * @todo angles0 initialisation must depend of omega0, chi0 etc...
   *  for now I am solving the problem by using Range::set_current(double) instead of Range::set_current(hkl::Value)
   *  in geometry conversion.
   */
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->setAngles(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(angle0));
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_EQUAL(fmod(angle0, hkl::constant::math::pi), fmod(angle, hkl::constant::math::pi));
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q2th(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  // this pseudoAxe is always readable.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if not initialize
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = _geometry->get_source().get_waveLength().get_value();
  double theta = 34 / 2;
  double value = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  pseudoAxe.set_current(value);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  // if put a non valid geometry can not set the value.
  _geometry->setAngles(40. * hkl::constant::math::degToRad,
                       72. * hkl::constant::math::degToRad,
                       -1. * hkl::constant::math::degToRad,
                       30. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // test the set_write_from_read
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
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
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
                       34. * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

  // random test
  /**
   * @todo angles0 initialisation must depend of omega0, chi0 etc...
   * for now I am solving the problem by using Range::set_current(double) instead of Range::set_current(hkl::Value)
   * in geometry conversion.
   */
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->setAngles(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double theta = ((max - min) * rand() / (RAND_MAX + 1.) + min) / 2.;
          double q0 = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
          std::cout << "i, j, q0 : " << i << " " << j << " " << q0 << std::endl;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(q0));
          double q = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // this pseudoAxe is always redeable
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if not initialize.
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  _geometry->get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  _geometry->setAngles(45 * hkl::constant::math::degToRad,
                       10 * hkl::constant::math::degToRad,
                       11 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  //set_current
  double lambda = _geometry->get_source().get_waveLength().get_value();
  double theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  // test the set_write_from_read
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
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
  _geometry->setAngles(45. * hkl::constant::math::degToRad,
                       77. * hkl::constant::math::degToRad,
                       -5. * hkl::constant::math::degToRad,
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
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->setAngles(omega0, chi0, phi0, tth0);
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
}

void
PseudoAxe_Eulerian4C_Vertical_Test::persistanceIO(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi psi_ref(*_geometry, _samples);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi psi(*_geometry, _samples);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th th2th_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th th2th(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th q2th_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th q2th(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q q_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q q(*_geometry);
  stringstream flux;

  psi_ref.toStream(flux);
  th2th_ref.toStream(flux);
  q2th_ref.toStream(flux);
  q_ref.toStream(flux);

  psi.fromStream(flux);
  th2th.fromStream(flux);
  q2th.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
  CPPUNIT_ASSERT_EQUAL(q2th_ref, q2th);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
