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
// File to test quaternion implementation.
#include "axe_rotation_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( AxeRotationTest );

void
AxeRotationTest::setUp(void) {}

void
AxeRotationTest::tearDown(void) {}

void
AxeRotationTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(hkl::axe::Rotation("toto", "titi", 1, 2, 3, hkl::svector(0., 0., 0)), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(hkl::axe::Rotation("toto", "titi", 1, 2, 3, hkl::svector(0., 0., 1)));

  // 1st constructor
  hkl::axe::Rotation rotation("toto", "titi", 1, 2, 3, hkl::svector(0., 0., 1));
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 1.), rotation.get_axe());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2, hkl::svector(0., 0., 1.)), rotation.get_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2, hkl::svector(0., 0., 1.)), rotation.get_quaternion_consign());

  // copy constructor
  hkl::axe::Rotation rotation1(rotation);
  CPPUNIT_ASSERT_EQUAL(rotation, rotation1);

  // clone
  hkl::axe::Rotation * rotation2 = static_cast<hkl::axe::Rotation *>(rotation.clone());
  CPPUNIT_ASSERT_EQUAL(rotation, *rotation2);
  delete rotation2;
}

void
AxeRotationTest::set(void)
{
  hkl::axe::Rotation axe("toto", "titi", 1, 2, 3, hkl::svector(0., 0., 1));

  // set the current value of the axe
  axe.set_current(2.5);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.), axe.get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2.5, hkl::svector(0., 0., 1.)), axe.get_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2., hkl::svector(0., 0., 1.)), axe.get_quaternion_consign());

  // set the consign of the axe.
  axe.set_consign(2.5);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2.5, hkl::svector(0., 0., 1.)), axe.get_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(2.5, hkl::svector(0., 0., 1.)), axe.get_quaternion_consign());
}

void
AxeRotationTest::get_distance(void)
{
  hkl::axe::Rotation A("toto", "titi", -2*hkl::constant::math::pi, 10 * hkl::constant::math::degToRad, 2*hkl::constant::math::pi, hkl::svector(0., 0., 1.));
  hkl::axe::Rotation B("toto", "titi", -2*hkl::constant::math::pi, -10 * hkl::constant::math::degToRad, 2*hkl::constant::math::pi, hkl::svector(0., 0., 1.));

  // get_distance
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(90 * hkl::constant::math::degToRad);
  B.set_current(-90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_current(120 * hkl::constant::math::degToRad);
  B.set_current(-150 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_current(-240 * hkl::constant::math::degToRad);
  B.set_current(200 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_current(200 * hkl::constant::math::degToRad);
  B.set_current(240 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_current(-90 * hkl::constant::math::degToRad);
  B.set_current(-100 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  // get_distance_consign
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_consign(90 * hkl::constant::math::degToRad);
  B.set_consign(-90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_consign(120 * hkl::constant::math::degToRad);
  B.set_consign(-150 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_consign(-240 * hkl::constant::math::degToRad);
  B.set_consign(200 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_consign(200 * hkl::constant::math::degToRad);
  B.set_consign(240 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);

  A.set_consign(-90 * hkl::constant::math::degToRad);
  B.set_consign(-100 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance_consign(B), hkl::constant::math::epsilon);
}

void
AxeRotationTest::persistanceIO(void)
{
  hkl::axe::Rotation axe_ref("toto", "titi", -hkl::constant::math::pi, 10 * hkl::constant::math::degToRad, hkl::constant::math::pi, hkl::svector(0, 0, 1));
  hkl::axe::Rotation axe("toto", "titi", -hkl::constant::math::pi, -10 * hkl::constant::math::degToRad, hkl::constant::math::pi, hkl::svector(1, 0, 1));

  hkl::axe::Rotation axe1_ref("toto", "titi", -hkl::constant::math::pi, 1 * hkl::constant::math::degToRad, hkl::constant::math::pi, hkl::svector(1, 0, 1));
  hkl::axe::Rotation axe1("toto", "tutu", -hkl::constant::math::pi, 2 * hkl::constant::math::degToRad, hkl::constant::math::pi, hkl::svector(1, 0, 1));

  std::stringstream flux;
  axe_ref.toStream(flux);
  axe1_ref.toStream(flux);
  axe.fromStream(flux);
  axe1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(axe_ref, axe);
  CPPUNIT_ASSERT_EQUAL(axe1_ref, axe1);
}
