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
#include "pseudoaxe_twoC_test.h"
#include "pseudoaxe.h"
#include <fstream>

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
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(1 * hkl::constant::math::degToRad));
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
  _geometry->set_angles(45 * hkl::constant::math::degToRad,
                        34 * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(45 * hkl::constant::math::degToRad,
                                34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(34. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_consign());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(36. * hkl::constant::math::degToRad));
  //  current must not move
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());
  //  consign must be computed.
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(angle0, angle_c, hkl::constant::math::epsilon);
        }
    }

#ifdef PROFILE
  // profiling
  hkl::Value v(36. * hkl::constant::math::degToRad);
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = _geometry->get_source().get_waveLength().get_value();
  double theta = 34. / 2.;
  double value = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
  _geometry->set_angles(45 * hkl::constant::math::degToRad,
                        34 * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(45 * hkl::constant::math::degToRad,
                                34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_consign());

  //set_current
  theta = 36. / 2.;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  pseudoAxe.set_consign(value);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

  // if put a non valid geometry can not set the value.
  _geometry->set_angles_consign(40. * hkl::constant::math::degToRad,
                                30. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_consign(1. * hkl::constant::math::degToRad), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // random test
  unsigned int i;
  unsigned int j;
  for (i=0;i<10;i++)
    {
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q2th0, q2th_c, hkl::constant::math::epsilon);
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
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize has no effect
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  _geometry->set_angles(45 * hkl::constant::math::degToRad, 34 * hkl::constant::math::degToRad);
  _geometry->set_angles_consign(45 * hkl::constant::math::degToRad, 34 * hkl::constant::math::degToRad);
  double lambda = _geometry->get_source().get_waveLength().get_value();
  double theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_consign());

  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_consign(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad), _geometry->omega()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad), _geometry->tth()->get_consign());

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
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, hkl::constant::math::epsilon);
          CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q_c, hkl::constant::math::epsilon);
        }
    }
}

void
PseudoAxe_TwoC_Vertical_Test::persistanceIO(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Th2th th2th_ref(*_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Th2th th2th(*_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q2th q2th_ref(*_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q2th q2th(*_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q q_ref(*_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q q(*_geometry);

  std::stringstream flux;

  th2th_ref.toStream(flux);
  q2th_ref.toStream(flux);
  q_ref.toStream(flux);

  th2th.fromStream(flux);
  q2th.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
  CPPUNIT_ASSERT_EQUAL(q2th_ref, q2th);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
