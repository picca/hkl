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
#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{
  //_geometry->get_source().setWaveLength(1.54);
  _geometry = new hkl::eulerian4C::vertical::Geometry;
  _geometry->set_angles(45. * hkl::constant::math::degToRad,
                        77. * hkl::constant::math::degToRad,
                        -5. * hkl::constant::math::degToRad,
                        34. * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(45. * hkl::constant::math::degToRad,
                                77. * hkl::constant::math::degToRad,
                                -5. * hkl::constant::math::degToRad,
                                34. * hkl::constant::math::degToRad);

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
  double angle = 10. * hkl::constant::math::degToRad;
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi pseudoAxeEngine(*_geometry, _samples);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["psi"];

  // test the initial stat of the pseudoAxe
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_min(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_max(), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1.), hkl::HKLException);


  // now initialize the pseudoAxe.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_initialized());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-hkl::constant::math::pi), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(hkl::constant::math::pi), pseudoAxe.get_max());
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(77 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-5 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  //set_current test2 degenerate case
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(30 * hkl::constant::math::degToRad,
                                                0 * hkl::constant::math::degToRad,
                                                0 * hkl::constant::math::degToRad,
                                                60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(30 * hkl::constant::math::degToRad,
                                                        0 * hkl::constant::math::degToRad,
                                                        0 * hkl::constant::math::degToRad,
                                                        60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->chi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0 * hkl::constant::math::degToRad), _geometry->phi()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60 * hkl::constant::math::degToRad), _geometry->tth()->get_current());

  // exception if the current geometry is not compatible with the initialization
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(1, 0, 0, 0));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(1, 0, 0, 0));
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_current(), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(pseudoAxe.get_consign(), hkl::HKLException);
  // the pseudoAxe must be non-writable
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());

  // test the set_write_from_read
  _geometry->set_angles(45. * hkl::constant::math::degToRad,
                        77. * hkl::constant::math::degToRad,
                        -5. * hkl::constant::math::degToRad,
                        34. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());

  //random test1
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(45 * hkl::constant::math::degToRad,
                                                77 * hkl::constant::math::degToRad,
                                                180 * hkl::constant::math::degToRad,
                                                34 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(45 * hkl::constant::math::degToRad,
                                                        77 * hkl::constant::math::degToRad,
                                                        180 * hkl::constant::math::degToRad,
                                                        34 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(angle));
      // the pseudoAxe must be writable and readable
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

  //random test2
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles(30 * hkl::constant::math::degToRad,
                                                0 * hkl::constant::math::degToRad,
                                                0 * hkl::constant::math::degToRad,
                                                60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(_geometry->set_angles_consign(30 * hkl::constant::math::degToRad,
                                                        0 * hkl::constant::math::degToRad,
                                                        0 * hkl::constant::math::degToRad,
                                                        60 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  for (i=-180;i<180;i++)
    {
      angle = i * hkl::constant::math::degToRad;
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
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle_c, hkl::constant::math::epsilon);
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
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th_c, hkl::constant::math::epsilon);
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
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double chi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double phi0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q_c, hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::persistanceIO(void)
{
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi psi_ref(*_geometry, _samples);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Psi psi(*_geometry, _samples);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th th2th_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th th2th(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th q2th_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th q2th(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q q_ref(*_geometry);
  hkl::eulerian4C::vertical::pseudoAxeEngine::Q q(*_geometry);
  stringstream flux;

  psi_ref.toStream(flux);
  th2th_ref.toStream(flux);
  q2th_ref.toStream(flux);
  q_ref.toStream(flux);

  psi.fromStream(flux);
  th2th.fromStream(flux);
  q2th.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
  CPPUNIT_ASSERT_EQUAL(q2th_ref, q2th);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
