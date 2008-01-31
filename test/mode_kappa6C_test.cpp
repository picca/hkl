/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "mode_kappa6C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Kappa6C_Test );

void
Mode_Kappa6C_Test::setUp(void)
{
  _geometry = new hkl::kappa6C::Geometry(50 * hkl::constant::math::degToRad);
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
Mode_Kappa6C_Test::tearDown(void)
{
  delete _sample;
  delete _geometry;
}

void
Mode_Kappa6C_Test::Bissector(void)
{
  hkl::smatrix UB = _sample->get_UB();

  hkl::kappa6C::mode::Bissector mode("test", "test", *_geometry);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-180. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(120. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-90. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  62.954883 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 134.755927 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-147.045165 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 177.0451647 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-134.755927 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( -32.9548353 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(135. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 45. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
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
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa6C_Test::Delta_Theta(void)
{
  hkl::smatrix UB = _sample->get_UB();

  hkl::kappa6C::mode::Delta_Theta mode("test", "test", *_geometry);
  mode.parameters()["delta theta"]->set_current(10 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 130 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-190. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(130. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 80. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 130. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-100. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(   0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  60. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(145. * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 35. * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(  0. * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 90. * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 1., UB), hkl::HKLException);

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
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
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa6C_Test::Constant_Omega(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa6C::mode::Constant_Omega mode("test", "test", *_geometry);
  double h, k, l;

  for (unsigned int i=0;i<1000;i++)
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
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa6C_Test::Constant_Chi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa6C::mode::Constant_Chi mode("test", "test", *_geometry);
  double h, k, l;

  for (unsigned int i=0;i<1000;i++)
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
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa6C_Test::Constant_Phi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::kappa6C::mode::Constant_Phi mode("test", "test", *_geometry);
  double h, k, l;

  for (unsigned int i=0;i<1000;i++)
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
      catch (hkl::HKLException) {}
    }
}

void
Mode_Kappa6C_Test::persistanceIO(void)
{
  hkl::kappa6C::mode::Bissector bissector_ref("test", "test", *_geometry), bissector("test", "test", *_geometry);
  hkl::kappa6C::mode::Delta_Theta delta_theta_ref("test", "test", *_geometry), delta_theta("test", "test", *_geometry);
  hkl::kappa6C::mode::Constant_Omega constant_omega_ref("test", "test", *_geometry), constant_omega("test", "test", *_geometry);
  hkl::kappa6C::mode::Constant_Chi constant_chi_ref("test", "test", *_geometry), constant_chi("test", "test", *_geometry);
  hkl::kappa6C::mode::Constant_Phi constant_phi_ref("test", "test", *_geometry), constant_phi("test", "test", *_geometry);
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
