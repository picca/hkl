#include "mode_kappa6C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Kappa6C_Test );

void
Mode_Kappa6C_Test::setUp(void)
{
  geometry::eulerian4C::Vertical geometry;

  _sample = new hkl::sample::MonoCrystal(_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(90 * constant::math::degToRad);
  lattice.gamma().set_current(90 * constant::math::degToRad);

  geometry.setAngles(30.*constant::math::degToRad,
                     0.*constant::math::degToRad,
                     90.*constant::math::degToRad,
                     60.*constant::math::degToRad);
  _geometry.setFromGeometry(geometry, true);
  _sample->reflections().add(svector(1., 0., 0.));

  geometry.setAngles(30.*constant::math::degToRad,
                     0.*constant::math::degToRad,
                     180.*constant::math::degToRad,
                     60.*constant::math::degToRad);
  _geometry.setFromGeometry(geometry, true);
  _sample->reflections().add(svector(0., 1., 0.));

  _sample->computeU(0, 1);
}

void
Mode_Kappa6C_Test::tearDown(void)
{
  delete _sample;
}

void
Mode_Kappa6C_Test::Bissector(void)
{
  smatrix UB = _sample->get_UB();

  mode::kappa6C::Bissector mode("test", "test", _geometry);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-60. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(180. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-60. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-60. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(270. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-60. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 90. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(   0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  -2.95484 * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-134.755927 * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 147.045165 * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(   0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(   0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-117.045165 * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 134.755927 * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  32.9548353 * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(   0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-45. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(225. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 90. * constant::math::degToRad), _geometry.delta()->get_current());

  // random test
  double h, k, l;
  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.computeHKL(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon);
        }
      catch (HKLException)
      {}
    }
}

void
Mode_Kappa6C_Test::Delta_Theta(void)
{
  smatrix UB = _sample->get_UB();

  mode::kappa6C::Delta_Theta mode("test", "test", _geometry);
  mode.parameters()["delta theta"]->set_current(10 * constant::math::degToRad);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-50. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-10. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-50. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(260. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-50. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 80. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 60. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.mu()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(-35. * constant::math::degToRad), _geometry.komega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.kappa()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(215. * constant::math::degToRad), _geometry.kphi()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(  0. * constant::math::degToRad), _geometry.gamma()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value( 90. * constant::math::degToRad), _geometry.delta()->get_current());

  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 1., UB), HKLException);

  // random test
  double h, k, l;
  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.computeHKL(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon);
        }
      catch (HKLException)
      {}
    }
}

void
Mode_Kappa6C_Test::Constant_Omega(void)
{
  smatrix UB = _sample->get_UB();
  mode::kappa6C::Constant_Omega mode("test", "test", _geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.computeHKL(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon);
        }
      catch (HKLException)
      {}
    }
}

void
Mode_Kappa6C_Test::Constant_Chi(void)
{
  smatrix UB = _sample->get_UB();
  mode::kappa6C::Constant_Chi mode("test", "test", _geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.computeHKL(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon);
        }
      catch (HKLException)
      {}
    }
}

void
Mode_Kappa6C_Test::Constant_Phi(void)
{
  smatrix UB = _sample->get_UB();
  mode::kappa6C::Constant_Phi mode("test", "test", _geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.computeHKL(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon);
        }
      catch (HKLException)
      {}
    }
}

void
Mode_Kappa6C_Test::persistanceIO(void)
{
  mode::kappa6C::Bissector bissector_ref("test", "test", _geometry), bissector("test", "test", _geometry);
  mode::kappa6C::Delta_Theta delta_theta_ref("test", "test", _geometry), delta_theta("test", "test", _geometry);
  mode::kappa6C::Constant_Omega constant_omega_ref("test", "test", _geometry), constant_omega("test", "test", _geometry);
  mode::kappa6C::Constant_Chi constant_chi_ref("test", "test", _geometry), constant_chi("test", "test", _geometry);
  mode::kappa6C::Constant_Phi constant_phi_ref("test", "test", _geometry), constant_phi("test", "test", _geometry);
  stringstream flux;

  bissector_ref.toStream(flux);
  delta_theta_ref.toStream(flux);
  constant_omega_ref.toStream(flux);
  constant_chi_ref.toStream(flux);
  constant_phi_ref.toStream(flux);
  bissector.fromStream(flux);
  delta_theta.fromStream(flux);
  constant_omega.fromStream(flux);
  constant_chi.fromStream(flux);
  constant_phi.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(bissector_ref, bissector);
  CPPUNIT_ASSERT_EQUAL(delta_theta_ref, delta_theta);
  CPPUNIT_ASSERT_EQUAL(constant_omega_ref, constant_omega);
  CPPUNIT_ASSERT_EQUAL(constant_chi_ref, constant_chi);
  CPPUNIT_ASSERT_EQUAL(constant_phi_ref, constant_phi);
}
