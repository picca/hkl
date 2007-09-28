#include "mode_kappa4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Kappa4C_Test );

void
Mode_Kappa4C_Test::setUp(void)
{
  hkl_svector hkl;

  _geometry = new hkl::kappa4C::vertical::Geometry(50 * HKL_DEGTORAD);
  hkl::eulerian4C::vertical::Geometry geometry;

  _sample = new hkl::sample::MonoCrystal(*_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * HKL_DEGTORAD);
  lattice.beta().set_current(90 * HKL_DEGTORAD);
  lattice.gamma().set_current(90 * HKL_DEGTORAD);

  geometry.set_angles(30.*HKL_DEGTORAD,
                      0.*HKL_DEGTORAD,
                      90.*HKL_DEGTORAD,
                      60.*HKL_DEGTORAD);
  _geometry->setFromGeometry(geometry, true);
  ::hkl_svector_set(&hkl, 1, 0, 0);
  _sample->reflections().add(&hkl);

  geometry.set_angles(30.*HKL_DEGTORAD,
                      0.*HKL_DEGTORAD,
                      180.*HKL_DEGTORAD,
                      60.*HKL_DEGTORAD);
  _geometry->setFromGeometry(geometry, true);
  ::hkl_svector_set(&hkl, 0, 1, 0);
  _sample->reflections().add(&hkl);

  _sample->computeU(0, 1);
}

void
Mode_Kappa4C_Test::tearDown(void)
{
  delete _sample;
  delete _geometry;
}

void
Mode_Kappa4C_Test::Bissector(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);

  hkl::kappa4C::vertical::mode::Bissector mode("test", "test", *_geometry);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 120. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-180. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-90. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  62.954883 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 134.755927 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-147.045165 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 177.0451647 * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-134.755927 * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( -32.9548353 * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(135. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 45. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry->compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa4C_Test::Delta_Theta(void)
{
  hkl_smatrix UB;
  hkl::kappa4C::vertical::mode::Delta_Theta mode("test", "test", *_geometry);
  mode.parameters()["delta theta"]->set_current(10 * HKL_DEGTORAD);

  _sample->get_UB(&UB);

  //CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  mode.computeAngles(-1., 0., 0., &UB);
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 130. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-190. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(130. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 80. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 130. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-100. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(145. * HKL_DEGTORAD), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * HKL_DEGTORAD), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 35. * HKL_DEGTORAD), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * HKL_DEGTORAD), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 1., &UB), hkl::HKLException);

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry->compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa4C_Test::Constant_Omega(void)
{
  double h, k, l;
  hkl_smatrix UB;
  hkl::kappa4C::vertical::mode::Constant_Omega mode("test", "test", *_geometry);

  _sample->get_UB(&UB);
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry->compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa4C_Test::Constant_Chi(void)
{
  double h, k, l;
  hkl_smatrix UB;
  hkl::kappa4C::vertical::mode::Constant_Chi mode("test", "test", *_geometry);

  _sample->get_UB(&UB);
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry->compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa4C_Test::Constant_Phi(void)
{
  double h, k, l;
  hkl_smatrix UB;
  hkl::kappa4C::vertical::mode::Constant_Phi mode("test", "test", *_geometry);

  _sample->get_UB(&UB);

  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry->compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException) {}
    }
}
