#include "mode_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Eulerian4C_Test );

using namespace hkl;

void
Mode_Eulerian4C_Test::setUp(void)
{
  hkl_svector hkl;

  _geometry.source.wave_length = 1.54;

  _sample = new hkl::sample::MonoCrystal(_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * HKL_DEGTORAD);
  lattice.beta().set_current(90 * HKL_DEGTORAD);
  lattice.gamma().set_current(90 * HKL_DEGTORAD);


  _geometry.set_angles(30.*HKL_DEGTORAD,
                       0.*HKL_DEGTORAD,
                       90.*HKL_DEGTORAD,
                       60.*HKL_DEGTORAD);
  ::hkl_svector_set(&hkl, 1, 0, 0);
  _sample->reflections().add(&hkl);

  _geometry.phi()->set_current(180.*HKL_DEGTORAD);
  ::hkl_svector_set(&hkl, 0, 1, 0);
  _sample->reflections().add(&hkl);

  _sample->computeU(0, 1);

  _geometry.set_angles(0, 0, 0, 0);
}

void
Mode_Eulerian4C_Test::tearDown(void)
{
  delete _sample;
}

void
Mode_Eulerian4C_Test::Bissector(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);

  hkl::eulerian4C::vertical::mode::Bissector mode("Bissector", "test", _geometry);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(90*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-90*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(180.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(90*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-90*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(90*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(45*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(135.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(h0, k0, l0, &UB));
      CPPUNIT_ASSERT_NO_THROW(_geometry.compute_HKL_consign(h, k, l, &UB));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
    }
}

void
Mode_Eulerian4C_Test::Delta_Theta(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);

  hkl::eulerian4C::vertical::mode::Delta_Theta mode("Delta Theta", "test", _geometry);
  mode.parameters()["delta theta"]->set_current(10 * HKL_DEGTORAD);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-100*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(170.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-10.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(Value(90*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(55*HKL_DEGTORAD), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*HKL_DEGTORAD), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(125.*HKL_DEGTORAD), _geometry.phi()->get_consign());

  //CPPUNIT_ASSERT_THROW( mode.computeAngles(0., 0., 1., UB, _geometry), HKLException);

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry.compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Omega(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);
  hkl::eulerian4C::vertical::mode::Constant_Omega mode("constant omega", "test", _geometry);
  mode.parameters()["omega"]->set_current(10 * HKL_DEGTORAD);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), HKLException);

  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry.compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Chi(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);
  hkl::eulerian4C::vertical::mode::Constant_Chi mode("constant chi", "test", _geometry);
  mode.parameters()["chi"]->set_current(45 * HKL_DEGTORAD);
  double h, k, l;

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), HKLException);

  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry.compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Phi(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);
  hkl::eulerian4C::vertical::mode::Constant_Phi mode("constant phi", "test", _geometry);
  double h, k, l;

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), HKLException);

  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, &UB);
          _geometry.compute_HKL_consign(h, k, l, &UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, HKL_EPSILON);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, HKL_EPSILON);
        }
      catch (hkl::HKLException const &) {}
    }
}
