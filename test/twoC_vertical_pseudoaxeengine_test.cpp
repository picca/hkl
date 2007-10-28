#include <fstream>

#include "config.h"
#include "pseudoaxe_twoC_test.h"
#include "pseudoaxe.h"

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_TwoC_Vertical_Test );

void
PseudoAxe_TwoC_Vertical_Test::setUp(void)
{
  _geometry = new hkl::twoC::vertical::Geometry;
}

void
PseudoAxe_TwoC_Vertical_Test::tearDown(void)
{
  delete _geometry;
}

void
PseudoAxe_TwoC_Vertical_Test::Th2th(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  // this pseudoAxe is always readable.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_consign());
  // but not writable if not initialized
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  // no more exception after initialization of the source.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_consign());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(1 * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  // set a non valid geometry an test
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(0, 1));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_consign());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  // test the uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe can be read all the time when the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  //set_current
  _geometry->set_angles(45 * HKL_DEGTORAD,
                        34 * HKL_DEGTORAD);
  _geometry->set_angles_consign(45 * HKL_DEGTORAD,
                                34 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(34. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * HKL_DEGTORAD), pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * HKL_DEGTORAD), pseudoAxe.get_consign());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(36. * HKL_DEGTORAD));
  //  current must not move
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_current());
  //  consign must be computed.
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(omega0, tth0);
      _geometry->set_angles_consign(omega0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(hkl::Value(angle0)));
          // set the current axes from the consign to test the update method.
          _geometry->omega()->set_current(_geometry->omega()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());

          double angle = pseudoAxe.get_current().get_value();
          double angle_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle_c, HKL_EPSILON);
        }
    }

#ifdef PROFILE
  // profiling
  hkl::Value v(36. * HKL_DEGTORAD);
  for (unsigned int i=0;i<1000000; i++)
    {
      pseudoAxe.set_current(v);
      pseudoAxe.get_current();
      pseudoAxe.get_min();
      pseudoAxe.get_max();
    }
#endif
}

void
PseudoAxe_TwoC_Vertical_Test::Q2th(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  // exception if not initialize
  // This pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_consign());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = _geometry->source.wave_length;
  double theta = 34. / 2.;
  double value = 2 * HKL_TAU * sin(theta * HKL_DEGTORAD) / lambda;
  _geometry->set_angles(45 * HKL_DEGTORAD,
                        34 * HKL_DEGTORAD);
  _geometry->set_angles_consign(45 * HKL_DEGTORAD,
                                34 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_consign());

  //set_current
  theta = 36. / 2.;
  value = 2 * HKL_TAU * sin(theta* HKL_DEGTORAD) / lambda;
  pseudoAxe.set_consign(value);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // if put a non valid geometry can not set the value.
  _geometry->set_angles_consign(40. * HKL_DEGTORAD,
                                30. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(omega0, tth0);
      _geometry->set_angles_consign(omega0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double q2th0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q2th0));
          // set the current axes from the consign to test the update method.
          _geometry->omega()->set_current(_geometry->omega()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());

          double q2th = pseudoAxe.get_current().get_value();
          double q2th_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th_c, HKL_EPSILON);
        }
    }
}

void
PseudoAxe_TwoC_Vertical_Test::Q(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Q pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // This pseudoAxe is always readable and writable.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  // uninitialize has no effect
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  //set_current
  _geometry->set_angles(45 * HKL_DEGTORAD, 34 * HKL_DEGTORAD);
  _geometry->set_angles_consign(45 * HKL_DEGTORAD, 34 * HKL_DEGTORAD);
  double lambda = _geometry->source.wave_length;
  double theta = 34 / 2 * HKL_DEGTORAD;
  double value = 2 * HKL_TAU * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_consign());

  //set_current
  theta = 36 / 2;
  value = 2 * HKL_TAU * sin(theta* HKL_DEGTORAD) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          double q0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q0));
          // set the current axes from the consign to test the update method.
          _geometry->tth()->set_current(_geometry->tth()->get_consign());

          double q = pseudoAxe.get_current().get_value();
          double q_c = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q_c, HKL_EPSILON);
        }
    }
}
