#include "mode_kappa4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Kappa4C_Test );

void
Mode_Kappa4C_Test::setUp(void)
{
  _geometry = new hkl::kappa4C::vertical::Geometry(50 * hkl::constant::math::degToRad);
  hkl::eulerian4C::vertical::Geometry geometry;

  _sample = new hkl::sample::MonoCrystal(*_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * hkl::constant::math::degToRad);
  lattice.beta().set_current(90 * hkl::constant::math::degToRad);
  lattice.gamma().set_current(90 * hkl::constant::math::degToRad);

  geometry.set_angles(30.*hkl::constant::math::degToRad,
                      0.*hkl::constant::math::degToRad,
                      90.*hkl::constant::math::degToRad,
                      60.*hkl::constant::math::degToRad);
  _geometry->setFromGeometry(geometry, true);
  _sample->reflections().add(hkl::svector(1., 0., 0.));

  geometry.set_angles(30.*hkl::constant::math::degToRad,
                      0.*hkl::constant::math::degToRad,
                      180.*hkl::constant::math::degToRad,
                      60.*hkl::constant::math::degToRad);
  _geometry->setFromGeometry(geometry, true);
  _sample->reflections().add(hkl::svector(0., 1., 0.));

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
  hkl::smatrix UB = _sample->get_UB();

  hkl::kappa4C::vertical::mode::Bissector mode("test", "test", *_geometry);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-180. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-90. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  62.954883 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 134.755927 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-147.045165 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 177.0451647 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-134.755927 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( -32.9548353 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(135. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 45. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

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
          _geometry->compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException)
      {}
    }
}

void
Mode_Kappa4C_Test::Delta_Theta(void)
{
  hkl::smatrix UB = _sample->get_UB();

  hkl::kappa4C::vertical::mode::Delta_Theta mode("test", "test", *_geometry);
  mode.parameters()["delta theta"]->set_current(10 * hkl::constant::math::degToRad);

  mode.computeAngles(-1., 0., 0., UB);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(130. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(170. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  mode.computeAngles(0., 1., 0., UB);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(130. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 80. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  mode.computeAngles(0.,-1., 0., UB);
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 130. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-100. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  mode.computeAngles(1., 1., 0., UB);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(145. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 35. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 1., UB), hkl::HKLException);

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
          _geometry->compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException)
      {}
    }
}

void
Mode_Kappa4C_Test::Constant_Omega(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa4C::vertical::mode::Constant_Omega mode("test", "test", *_geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry->compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException)
      {}
    }
}

void
Mode_Kappa4C_Test::Constant_Chi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa4C::vertical::mode::Constant_Chi mode("test", "test", *_geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry->compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException)
      {}
    }
}

void
Mode_Kappa4C_Test::Constant_Phi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa4C::vertical::mode::Constant_Phi mode("test", "test", *_geometry);
  double h, k, l;

  for(unsigned int i=0;i<1000;i++)
    {
      double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry->compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException)
      {}
    }
}

void
Mode_Kappa4C_Test::persistanceIO(void)
{
  hkl::kappa4C::vertical::mode::Bissector bissector_ref("test", "test", *_geometry), bissector("test", "test", *_geometry);
  hkl::kappa4C::vertical::mode::Delta_Theta delta_theta_ref("test", "test", *_geometry), delta_theta("test", "test", *_geometry);
  hkl::kappa4C::vertical::mode::Constant_Omega constant_omega_ref("test", "test", *_geometry), constant_omega("test", "test", *_geometry);
  hkl::kappa4C::vertical::mode::Constant_Chi constant_chi_ref("test", "test", *_geometry), constant_chi("test", "test", *_geometry);
  hkl::kappa4C::vertical::mode::Constant_Phi constant_phi_ref("test", "test", *_geometry), constant_phi("test", "test", *_geometry);
  std::stringstream flux;

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
