#include "pseudoaxe_kappa4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa4C_Vertical_Test );

void
PseudoAxe_Kappa4C_Vertical_Test::setUp(void)
{
  _geometry = new hkl::kappa4C::vertical::Geometry(50 * HKL_DEGTORAD);
  _geometry_E4C = new hkl::eulerian4C::vertical::Geometry;
  _samples = new hkl::SampleList(*_geometry);
}

void
PseudoAxe_Kappa4C_Vertical_Test::tearDown(void)
{
  delete _samples;
  delete _geometry_E4C;
  delete _geometry;
}

void
PseudoAxe_Kappa4C_Vertical_Test::Omega(void)
{
  _geometry->source.wave_length = 1.54;
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["omega"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3*M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2*M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3*M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2*M_PI), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2*M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));
  for (unsigned int i=-180;i<=180;i++)
    {
      double angle = i * HKL_DEGTORAD;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_current().get_value(), HKL_EPSILON);
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Chi(void)
{
  int i;
  double angle;

  _geometry->source.wave_length = 1.54;
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["chi"];
  int chi_max = 100;

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-_geometry->get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(_geometry->get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-_geometry->get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(_geometry->get_alpha() * 2.), pseudoAxe.get_max());
  // but un-writable after un-initialization
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-_geometry->get_alpha() * 2.), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(_geometry->get_alpha() * 2.), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  //test exception if chi > 2*alpha
  angle = chi_max + 0.1;
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(angle), hkl::HKLException);


  //!< @todo = chi_max
  for (i=-chi_max;i<=chi_max;i++)
    {
      angle = i * HKL_DEGTORAD;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      // set the current from the consign
      _geometry->komega()->set_current(_geometry->komega()->get_consign());
      _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
      _geometry->kphi()->set_current(_geometry->kphi()->get_consign());

      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_consign());
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Phi(void)
{
  int i;
  double angle;

  _geometry->source.wave_length = 1.54;
  hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["phi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2*M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(3*M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2*M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(3*M_PI), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2*M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(3*M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));
  for (i=-179;i<=180;i++)
    {
      angle = i * HKL_DEGTORAD;
      //CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      pseudoAxe.set_consign(angle);
      // set the current from the consign
      _geometry->komega()->set_current(_geometry->komega()->get_consign());
      _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
      _geometry->kphi()->set_current(_geometry->kphi()->get_consign());

      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
      CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_consign());
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Psi(void)
{
  int i;
  double angle = 10. * HKL_DEGTORAD;
  hkl::kappa4C::vertical::pseudoAxeEngine::Psi pseudoAxeEngine(*_geometry, _samples);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  _geometry_E4C->set_angles(45. * HKL_DEGTORAD,
                            77. * HKL_DEGTORAD,
                            -5. * HKL_DEGTORAD,
                            34. * HKL_DEGTORAD);
  _geometry_E4C->set_angles_consign(45. * HKL_DEGTORAD,
                                    77. * HKL_DEGTORAD,
                                    -5. * HKL_DEGTORAD,
                                    34. * HKL_DEGTORAD);
  _geometry->setFromGeometry(*_geometry_E4C, true);

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1.), hkl::HKLException);

  // now initialize the the pseudoAxe.
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+M_PI), pseudoAxe.get_max());
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
  _geometry_E4C->setFromGeometry(*_geometry, true);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45. * HKL_DEGTORAD), _geometry_E4C->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77. * HKL_DEGTORAD), _geometry_E4C->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5. * HKL_DEGTORAD), _geometry_E4C->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * HKL_DEGTORAD), _geometry_E4C->tth()->get_current());

  //set_current test2 degenerate case
  _geometry_E4C->set_angles(30. * HKL_DEGTORAD,
                            0. * HKL_DEGTORAD,
                            0. * HKL_DEGTORAD,
                            60. * HKL_DEGTORAD);
  _geometry_E4C->set_angles_consign(30. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    60. * HKL_DEGTORAD);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30. * HKL_DEGTORAD), _geometry_E4C->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * HKL_DEGTORAD), _geometry_E4C->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * HKL_DEGTORAD), _geometry_E4C->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60. * HKL_DEGTORAD), _geometry_E4C->tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  _geometry->set_angles(1, 0, 0, 0);
  _geometry->set_angles_consign(1, 0, 0, 0);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  //get_value test
  _geometry_E4C->set_angles(45. * HKL_DEGTORAD,
                            77. * HKL_DEGTORAD,
                            180. * HKL_DEGTORAD,
                            34. * HKL_DEGTORAD);
  _geometry_E4C->set_angles_consign(45. * HKL_DEGTORAD,
                                    77. * HKL_DEGTORAD,
                                    180. * HKL_DEGTORAD,
                                    34. * HKL_DEGTORAD);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * HKL_DEGTORAD;
      if ((i <= -174) || (i >= 47))
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
          _geometry->komega()->set_current(_geometry->komega()->get_consign());
          _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
          _geometry->kphi()->set_current(_geometry->kphi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
          CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_consign());
        }
    }

  _geometry_E4C->set_angles(30. * HKL_DEGTORAD,
                            0. * HKL_DEGTORAD,
                            0. * HKL_DEGTORAD,
                            60. * HKL_DEGTORAD);
  _geometry_E4C->set_angles_consign(30. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    60. * HKL_DEGTORAD);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * HKL_DEGTORAD;
      if (abs(i) > 100)
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
          _geometry->komega()->set_current(_geometry->komega()->get_consign());
          _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
          _geometry->kphi()->set_current(_geometry->kphi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(hkl::Value(angle), pseudoAxe.get_current());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), HKL_EPSILON);
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Th2th(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  //set_current
  _geometry->set_angles(45. * HKL_DEGTORAD,
                        77. * HKL_DEGTORAD,
                        -5. * HKL_DEGTORAD,
                        34. * HKL_DEGTORAD);
  _geometry->set_angles_consign(45. * HKL_DEGTORAD,
                                77. * HKL_DEGTORAD,
                                -5. * HKL_DEGTORAD,
                                34. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(34. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * HKL_DEGTORAD), _geometry->kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * HKL_DEGTORAD), _geometry->kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * HKL_DEGTORAD), pseudoAxe.get_current());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(36. * HKL_DEGTORAD));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // if put a non valid geometry can not set the value.
  _geometry->set_angles(40. * HKL_DEGTORAD,
                        72. * HKL_DEGTORAD,
                        -1. * HKL_DEGTORAD,
                        30. * HKL_DEGTORAD);
  _geometry->set_angles_consign(40. * HKL_DEGTORAD,
                                72. * HKL_DEGTORAD,
                                -1. * HKL_DEGTORAD,
                                30. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(_geometry->tth()->get_max(), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double komega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(komega0, kappa0, kphi0, tth0);
      _geometry->set_angles_consign(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          hkl::Value angle0( (max - min) * rand() / (RAND_MAX + 1.) + min );
          pseudoAxe.set_consign(angle0);
          _geometry->komega()->set_current(_geometry->komega()->get_consign());
          _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
          _geometry->kphi()->set_current(_geometry->kphi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(angle0, pseudoAxe.get_current());
          CPPUNIT_ASSERT_EQUAL(angle0, pseudoAxe.get_consign());
          //double angle = pseudoAxe.get_current().get_value();
          //CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, M_PI), fmod(angle, M_PI), HKL_EPSILON);
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Q2th(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  // test the initial state
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  //set_current
  double lambda = _geometry->source.wave_length;
  double theta = 34 / 2;
  double value = 2 * HKL_TAU * sin(theta * HKL_DEGTORAD) / lambda;
  _geometry->set_angles(45. * HKL_DEGTORAD,
                        77. * HKL_DEGTORAD,
                        -5. * HKL_DEGTORAD,
                        34. * HKL_DEGTORAD);
  _geometry->set_angles_consign(45. * HKL_DEGTORAD,
                                77. * HKL_DEGTORAD,
                                -5. * HKL_DEGTORAD,
                                34. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * HKL_DEGTORAD), _geometry->kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * HKL_DEGTORAD), _geometry->kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * HKL_TAU * sin(theta* HKL_DEGTORAD) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // if put a non valid geometry can not get the value.
  _geometry->set_angles(40. * HKL_DEGTORAD,
                        72. * HKL_DEGTORAD,
                        -1. * HKL_DEGTORAD,
                        30. * HKL_DEGTORAD);
  _geometry->set_angles_consign(40. * HKL_DEGTORAD,
                                72. * HKL_DEGTORAD,
                                -1. * HKL_DEGTORAD,
                                30. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double komega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(komega0, kappa0, kphi0, tth0);
      _geometry->set_angles_consign(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          hkl::Value q2th( (max - min) * rand() / (RAND_MAX + 1.) + min );
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q2th));
          _geometry->komega()->set_current(_geometry->komega()->get_consign());
          _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
          _geometry->kphi()->set_current(_geometry->kphi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(q2th, pseudoAxe.get_current());
          CPPUNIT_ASSERT_EQUAL(q2th, pseudoAxe.get_consign());
        }
    }
}

void
PseudoAxe_Kappa4C_Vertical_Test::Q(void)
{
  hkl::kappa4C::vertical::pseudoAxeEngine::Q pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // no more exception initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(1. * HKL_DEGTORAD));

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * HKL_TAU / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * HKL_TAU / 1.54), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

  _geometry->set_angles(45 * HKL_DEGTORAD,
                        10 * HKL_DEGTORAD,
                        11 * HKL_DEGTORAD,
                        34 * HKL_DEGTORAD);
  _geometry->set_angles_consign(45 * HKL_DEGTORAD,
                                10 * HKL_DEGTORAD,
                                11 * HKL_DEGTORAD,
                                34 * HKL_DEGTORAD);
  //set_current
  double lambda = _geometry->source.wave_length;
  double theta = 34 / 2 * HKL_DEGTORAD;
  double value = 2 * HKL_TAU * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * HKL_DEGTORAD), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * HKL_TAU * sin(theta* HKL_DEGTORAD) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(10 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(11 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double komega0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = M_PI * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(komega0, kappa0, kphi0, tth0);
      _geometry->set_angles_consign(komega0, kappa0, kphi0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for (j=0;j<100;j++)
        {
          hkl::Value q( (max - min) * rand() / (RAND_MAX + 1.) + min );
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(q));
          _geometry->komega()->set_current(_geometry->komega()->get_consign());
          _geometry->kappa()->set_current(_geometry->kappa()->get_consign());
          _geometry->kphi()->set_current(_geometry->kphi()->get_consign());
          _geometry->tth()->set_current(_geometry->tth()->get_consign());
          CPPUNIT_ASSERT_EQUAL(q, pseudoAxe.get_current());
          CPPUNIT_ASSERT_EQUAL(q, pseudoAxe.get_consign());
        }
    }
}
