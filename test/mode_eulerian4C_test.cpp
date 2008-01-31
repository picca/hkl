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
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "mode_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Eulerian4C_Test );

using namespace hkl;

void
Mode_Eulerian4C_Test::setUp(void)
{
  _geometry.get_source().setWaveLength(1.54);

  _sample = new hkl::sample::MonoCrystal(_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(90 * constant::math::degToRad);
  lattice.gamma().set_current(90 * constant::math::degToRad);


  _geometry.set_angles(30.*constant::math::degToRad,
                       0.*constant::math::degToRad,
                       90.*constant::math::degToRad,
                       60.*constant::math::degToRad);
  _sample->reflections().add(svector(1., 0., 0.));

  _geometry.phi()->set_current(180.*constant::math::degToRad);
  _sample->reflections().add(svector(0., 1., 0.));

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
  hkl::smatrix UB = _sample->get_UB();

  hkl::eulerian4C::vertical::mode::Bissector mode("Bissector", "test", _geometry);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(90*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-90*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(180.*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(90*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-90*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(90*constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(45*constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(135.*constant::math::degToRad), _geometry.phi()->get_consign());

  // random test
  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(h0, k0, l0, UB));
      CPPUNIT_ASSERT_NO_THROW(_geometry.compute_HKL_consign(h, k, l, UB));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
    }
}

void
Mode_Eulerian4C_Test::Delta_Theta(void)
{
  hkl::smatrix UB = _sample->get_UB();

  hkl::eulerian4C::vertical::mode::Delta_Theta mode("Delta Theta", "test", _geometry);
  mode.parameters()["delta theta"]->set_current(10 * constant::math::degToRad);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*hkl::constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*hkl::constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*hkl::constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-100*hkl::constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*hkl::constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*hkl::constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*hkl::constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(170.*hkl::constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*hkl::constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(40*hkl::constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0.*hkl::constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(-10.*hkl::constant::math::degToRad), _geometry.phi()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(90*hkl::constant::math::degToRad), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(55*hkl::constant::math::degToRad), _geometry.omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(0*hkl::constant::math::degToRad), _geometry.chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(Value(125.*hkl::constant::math::degToRad), _geometry.phi()->get_consign());

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
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Omega(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::eulerian4C::vertical::mode::Constant_Omega mode("constant omega", "test", _geometry);
  mode.parameters()["omega"]->set_current(10 * hkl::constant::math::degToRad);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);

  double h, k, l;
  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Chi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::eulerian4C::vertical::mode::Constant_Chi mode("constant chi", "test", _geometry);
  mode.parameters()["chi"]->set_current(45 * hkl::constant::math::degToRad);
  double h, k, l;

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);

  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::Constant_Phi(void)
{
  hkl::smatrix UB = _sample->get_UB();
  hkl::eulerian4C::vertical::mode::Constant_Phi mode("constant phi", "test", _geometry);
  double h, k, l;

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);

  for (unsigned int i=0;i<1000;i++)
    {
      double h0 = 2. * (rand() / (RAND_MAX + 1.) - .5);
      double k0 = sqrt(4.-h0*h0) * (rand() / (RAND_MAX + 1.) - .5);
      double l0 = sqrt(4.-h0*h0-k0*k0) * (rand() / (RAND_MAX + 1.) - .5);

      try
        {
          mode.computeAngles(h0, k0, l0, UB);
          _geometry.compute_HKL_consign(h, k, l, UB);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, hkl::constant::math::epsilon);
        }
      catch (hkl::HKLException const &) {}
    }
}

void
Mode_Eulerian4C_Test::persistanceIO(void)
{
  hkl::eulerian4C::vertical::mode::Bissector bissector_ref("Bissector", "test", _geometry), bissector("xxx", "xxx", _geometry);
  hkl::eulerian4C::vertical::mode::Delta_Theta delta_theta_ref("Delta Theta", "test", _geometry), delta_theta("xxx", "xxx", _geometry);
  hkl::eulerian4C::vertical::mode::Constant_Omega constant_omega_ref("Constant omega", "test", _geometry), constant_omega("xxx", "xxx", _geometry);
  hkl::eulerian4C::vertical::mode::Constant_Chi constant_chi_ref("Constant chi", "test", _geometry), constant_chi("xxx", "xxx", _geometry);
  hkl::eulerian4C::vertical::mode::Constant_Phi constant_phi_ref("Constant phi", "test", _geometry), constant_phi("xxx", "xxx", _geometry);
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
