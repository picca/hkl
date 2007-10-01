#include <fstream>

#include "config.h"
#include "pseudoaxe_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{
  //_geometry->get_source().setWaveLength(1.54);
  _geometry = new hkl::eulerian4C::vertical::Geometry;
  _geometry->set_angles(45. * HKL_DEGTORAD,
                        77. * HKL_DEGTORAD,
                        -5. * HKL_DEGTORAD,
                        34. * HKL_DEGTORAD);
  _geometry->set_angles_consign(45. * HKL_DEGTORAD,
                                77. * HKL_DEGTORAD,
                                -5. * HKL_DEGTORAD,
                                34. * HKL_DEGTORAD);

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
  double angle = 10. * HKL_DEGTORAD;
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi pseudoAxeEngine(*_geometry, _samples);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1.), hkl::HKLException);

  // now initialize the pseudoAxe.
  pseudoAxe.initialize();
  //CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * HKL_DEGTORAD));

  // test the uninitialized state
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1.), hkl::HKLException);

  //set_current test1 non degenerate case
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * HKL_DEGTORAD), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * HKL_DEGTORAD), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(30 * HKL_DEGTORAD,
                                                0 * HKL_DEGTORAD,
                                                0 * HKL_DEGTORAD,
                                                60 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(30 * HKL_DEGTORAD,
                                                        0 * HKL_DEGTORAD,
                                                        0 * HKL_DEGTORAD,
                                                        60 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30 * HKL_DEGTORAD), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * HKL_DEGTORAD), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * HKL_DEGTORAD), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60 * HKL_DEGTORAD), _geometry->tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(1, 0, 0, 0));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(1, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_consign(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  // test the set_write_from_read
  _geometry->set_angles(45. * HKL_DEGTORAD,
                        77. * HKL_DEGTORAD,
                        -5. * HKL_DEGTORAD,
                        34. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());

  //random test1
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(45 * HKL_DEGTORAD,
                                                77 * HKL_DEGTORAD,
                                                180 * HKL_DEGTORAD,
                                                34 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(45 * HKL_DEGTORAD,
                                                        77 * HKL_DEGTORAD,
                                                        180 * HKL_DEGTORAD,
                                                        34 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-179;i<180;i++)
    {
      angle = i * HKL_DEGTORAD;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      // the pseudoAxe must be writable and readable
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_consign());

      // set the current values of axes from the consign to test the update method
      _geometry->omega()->set_current(_geometry->omega()->get_consign());
      _geometry->chi()->set_current(_geometry->chi()->get_consign());
      _geometry->phi()->set_current(_geometry->phi()->get_consign());
      _geometry->tth()->set_current(_geometry->tth()->get_consign());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
    }

  //random test2
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(30 * HKL_DEGTORAD,
                                                0 * HKL_DEGTORAD,
                                                0 * HKL_DEGTORAD,
                                                60 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(30 * HKL_DEGTORAD,
                                                        0 * HKL_DEGTORAD,
                                                        0 * HKL_DEGTORAD,
                                                        60 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-179;i<180;i++)
    {
      angle = i * HKL_DEGTORAD;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      // set the current values of axes from the consign to test the update method
      _geometry->omega()->set_current(_geometry->omega()->get_consign());
      _geometry->chi()->set_current(_geometry->chi()->get_consign());
      _geometry->phi()->set_current(_geometry->phi()->get_consign());
      _geometry->tth()->set_current(_geometry->tth()->get_consign());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_consign());
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Th2th(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(omega0, chi0, phi0, tth0);
      _geometry->set_angles_consign(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle0));
          // set the current axes from the consign to test the update method.
          _geometry->omega()->set_current(_geometry->omega()->get_consign());
          _geometry->chi()->set_current(_geometry->chi()->get_consign());
          _geometry->phi()->set_current(_geometry->phi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

          double angle = pseudoAxe.get_current().get_value();
          double angle_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle_c, HKL_EPSILON);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q2th(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(omega0, chi0, phi0, tth0);
      _geometry->set_angles_consign(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double q2th0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q2th0));
          // set the current axes from the consign to test the update method.
          _geometry->omega()->set_current(_geometry->omega()->get_consign());
          _geometry->chi()->set_current(_geometry->chi()->get_consign());
          _geometry->phi()->set_current(_geometry->phi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

          double q2th = pseudoAxe.get_current().get_value();
          double q2th_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th_c, HKL_EPSILON);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::Q(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(omega0, chi0, phi0, tth0);
      _geometry->set_angles_consign(omega0, chi0, phi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
      CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double q0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q0));
          // set the current axes from the consign to test the update method.
          _geometry->omega()->set_current(_geometry->omega()->get_consign());
          _geometry->chi()->set_current(_geometry->chi()->get_consign());
          _geometry->phi()->set_current(_geometry->phi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
          CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

          double q = pseudoAxe.get_current().get_value();
          double q_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q_c, HKL_EPSILON);
        }
    }
}
