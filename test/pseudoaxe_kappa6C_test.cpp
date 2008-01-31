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
#include "pseudoaxe_kappa6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa6C_Test );

void
PseudoAxe_Kappa6C_Test::setUp(void)
{
  _alpha = 50 * hkl::constant::math::degToRad;
  _geometry_E4C = new hkl::eulerian4C::vertical::Geometry;
  _geometry_E6C = new hkl::eulerian6C::Geometry;
  _geometry_K4C = new hkl::kappa4C::vertical::Geometry(_alpha);
  _geometry = new hkl::kappa6C::Geometry(_alpha);

  _samples = new hkl::SampleList(*_geometry);
}

void
PseudoAxe_Kappa6C_Test::tearDown(void)
{
  delete _samples;
  delete _geometry;
  delete _geometry_K4C;
  delete _geometry_E6C;
  delete _geometry_E4C;
}

void
PseudoAxe_Kappa6C_Test::Omega(void)
{
  int i;
  double angle;
  _geometry->get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry, _alpha);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["omega"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 2 * M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 2 * M_PI), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-3 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 2 * M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // random test
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), hkl::constant::math::epsilon);
    }
}

void
PseudoAxe_Kappa6C_Test::Chi(void)
{
  int i;
  double angle;
  _geometry->get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry, _alpha);
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

  for (i=-chi_max;i<chi_max;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), hkl::constant::math::epsilon);
    }
}

void
PseudoAxe_Kappa6C_Test::Phi(void)
{
  int i;
  double angle;
  _geometry->get_source().setWaveLength(1.54);
  hkl::kappa6C::pseudoAxeEngine::Eulerians pseudoAxeEngine(*_geometry, _alpha);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["phi"];

  // test the initial state of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 3 * M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));

  // uninitialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe is always readable
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 3 * M_PI), pseudoAxe.get_max());
  // after uninitialization no write possible.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0.), hkl::HKLException);

  // initialize it
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * M_PI), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value( 3 * M_PI), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), hkl::constant::math::epsilon);
    }
}

void
PseudoAxe_Kappa6C_Test::Psi(void)
{
  int i;
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::kappa6C::pseudoAxeEngine::Psi pseudoAxeEngine(*_geometry, _samples);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  _geometry_E4C->set_angles(45. * hkl::constant::math::degToRad,
                            77. * hkl::constant::math::degToRad,
                            -5. * hkl::constant::math::degToRad,
                            34. * hkl::constant::math::degToRad);
  _geometry_E4C->set_angles_consign(45. * hkl::constant::math::degToRad,
                                    77. * hkl::constant::math::degToRad,
                                    -5. * hkl::constant::math::degToRad,
                                    34. * hkl::constant::math::degToRad);
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * hkl::constant::math::degToRad));

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
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * hkl::constant::math::degToRad));
  _geometry_E4C->setFromGeometry(*_geometry, true);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45. * hkl::constant::math::degToRad), _geometry_E4C->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77. * hkl::constant::math::degToRad), _geometry_E4C->chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5. * hkl::constant::math::degToRad), _geometry_E4C->phi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), _geometry_E4C->tth()->get_consign());

  //set_current test2 degenerate case
  _geometry_E4C->set_angles(30. * hkl::constant::math::degToRad,
                            0. * hkl::constant::math::degToRad,
                            0. * hkl::constant::math::degToRad,
                            60. * hkl::constant::math::degToRad);
  _geometry_E4C->set_angles_consign(30. * hkl::constant::math::degToRad,
                                    0. * hkl::constant::math::degToRad,
                                    0. * hkl::constant::math::degToRad,
                                    60. * hkl::constant::math::degToRad);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30. * hkl::constant::math::degToRad), _geometry_E4C->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * hkl::constant::math::degToRad), _geometry_E4C->chi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0. * hkl::constant::math::degToRad), _geometry_E4C->phi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60. * hkl::constant::math::degToRad), _geometry_E4C->tth()->get_consign());

  // exception if the current geometry is not compatible with the initialization
  _geometry->set_angles(0, 1, 0, 0, 0, 0);
  _geometry->set_angles_consign(0, 1, 0, 0, 0, 0);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_consign(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  //get_value test
  _geometry_E4C->set_angles(45. * hkl::constant::math::degToRad,
                            77. * hkl::constant::math::degToRad,
                            180. * hkl::constant::math::degToRad,
                            34. * hkl::constant::math::degToRad);
  _geometry_E4C->set_angles_consign(45. * hkl::constant::math::degToRad,
                                    77. * hkl::constant::math::degToRad,
                                    180. * hkl::constant::math::degToRad,
                                    34. * hkl::constant::math::degToRad);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      if ((i <= -174) || (i >= 47))
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), hkl::constant::math::epsilon);
        }
    }

  _geometry_E4C->set_angles(30. * hkl::constant::math::degToRad,
                            0. * hkl::constant::math::degToRad,
                            0. * hkl::constant::math::degToRad,
                            60. * hkl::constant::math::degToRad);
  _geometry_E4C->set_angles_consign(30. * hkl::constant::math::degToRad,
                                    0. * hkl::constant::math::degToRad,
                                    0. * hkl::constant::math::degToRad,
                                    60. * hkl::constant::math::degToRad);
  _geometry->setFromGeometry(*_geometry_E4C, true);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      if (abs(i) > 100)
        {
          CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(angle), hkl::HKLException);
        }
      else
        {
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_consign().get_value(), hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_Kappa6C_Test::Tth(void)
{
  hkl::kappa6C::pseudoAxeEngine::Tth pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["tth"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);


  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(34. * hkl::constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(+hkl::constant::math::pi), pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  //set_current
  _geometry->set_angles(1. * hkl::constant::math::degToRad,
                        45. * hkl::constant::math::degToRad,
                        77. * hkl::constant::math::degToRad,
                        -5. * hkl::constant::math::degToRad,
                        0. * hkl::constant::math::degToRad,
                        34. * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(1. * hkl::constant::math::degToRad,
                                45. * hkl::constant::math::degToRad,
                                77. * hkl::constant::math::degToRad,
                                -5. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(pseudoAxe.get_current()));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->delta()->get_consign());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  pseudoAxe.set_consign(36. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      _geometry->set_angles_consign(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      for (j=0;j<100;j++)
        {
          double angle0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle0));
          double angle = 0;
          CPPUNIT_ASSERT_NO_THROW(angle = pseudoAxe.get_consign().get_value());
          CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, hkl::constant::math::pi), fmod(angle, hkl::constant::math::pi), hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_Kappa6C_Test::Q(void)
{
  hkl::kappa6C::pseudoAxeEngine::Q pseudoAxeEngine(*_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // test the initial state
  // no exception the pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  double lambda = _geometry->get_source().get_waveLength().get_value();
  double theta = M_PI_2;
  hkl::Value min(-2 * hkl::constant::physic::tau * sin(theta) / lambda);
  hkl::Value max( 2 * hkl::constant::physic::tau * sin(theta) / lambda);
  CPPUNIT_ASSERT_EQUAL(min, pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(max, pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  // no more exception after a correct initialization
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(min, pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(max, pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(34. * hkl::constant::math::degToRad));

  // test the uninitialize method
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(min, pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(max, pseudoAxe.get_max());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1), hkl::HKLException);

  //set_current
  _geometry->set_angles(1. * hkl::constant::math::degToRad,
                        45. * hkl::constant::math::degToRad,
                        77. * hkl::constant::math::degToRad,
                        -5. * hkl::constant::math::degToRad,
                        0. * hkl::constant::math::degToRad,
                        34. * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(1. * hkl::constant::math::degToRad,
                                45. * hkl::constant::math::degToRad,
                                77. * hkl::constant::math::degToRad,
                                -5. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->delta()->get_consign());
  //get_current
  CPPUNIT_ASSERT_EQUAL(hkl::Value((double)value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1 * hkl::constant::math::degToRad), _geometry->mu()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->komega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->kappa()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->kphi()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->gamma()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->delta()->get_consign());

  // peticular test
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0.));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), pseudoAxe.get_consign());

  // test the writable change if the geometry is not compatible with the pseudoAxe initialization
  // in this case no effect.
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  _geometry->set_angles(0, 0, 0, 0, 0, 0);
  _geometry->set_angles_consign(0, 0, 0, 0, 0, 0);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_consign());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<100;i++)
    {
      double mu0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double komega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kappa0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double kphi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double gamma0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double delta0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      _geometry->set_angles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      _geometry->set_angles_consign(mu0, komega0, kappa0, kphi0, gamma0, delta0);
      pseudoAxe.initialize();
      for (j=0;j<100;j++)
        {
          double theta = hkl::constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
          double q0 = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
          pseudoAxe.set_consign(q0);
          double q = pseudoAxe.get_consign().get_value();
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_Kappa6C_Test::persistanceIO(void)
{
  hkl::kappa6C::pseudoAxeEngine::Psi psi_ref(*_geometry, _samples);
  hkl::kappa6C::pseudoAxeEngine::Psi psi(*_geometry, _samples);
  hkl::kappa6C::pseudoAxeEngine::Eulerians eulerians_ref(*_geometry, _alpha);
  hkl::kappa6C::pseudoAxeEngine::Eulerians eulerians(*_geometry, _alpha);
  hkl::kappa6C::pseudoAxeEngine::Tth tth_ref(*_geometry);
  hkl::kappa6C::pseudoAxeEngine::Tth tth(*_geometry);
  hkl::kappa6C::pseudoAxeEngine::Q q_ref(*_geometry);
  hkl::kappa6C::pseudoAxeEngine::Q q(*_geometry);
  std::stringstream flux;

  psi_ref.toStream(flux);
  eulerians_ref.toStream(flux);
  tth_ref.toStream(flux);
  q_ref.toStream(flux);

  psi.fromStream(flux);
  eulerians.fromStream(flux);
  tth.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
  CPPUNIT_ASSERT_EQUAL(eulerians_ref, eulerians);
  CPPUNIT_ASSERT_EQUAL(tth_ref, tth);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
