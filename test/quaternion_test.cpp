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
// File to test quaternion implementation.
#include "quaternion_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( QuaternionTest );

void
QuaternionTest::setUp(void) {}

void
QuaternionTest::tearDown(void) {}

void
QuaternionTest::Constructor1(void)
{
  Quaternion q;

  CPPUNIT_ASSERT_EQUAL( 1., q.a());
  CPPUNIT_ASSERT_EQUAL( 0., q.b());
  CPPUNIT_ASSERT_EQUAL( 0., q.c());
  CPPUNIT_ASSERT_EQUAL( 0., q.d());
}

void
QuaternionTest::Constructor2(void)
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_EQUAL( 1., q.a());
  CPPUNIT_ASSERT_EQUAL( 2., q.b());
  CPPUNIT_ASSERT_EQUAL( 3., q.c());
  CPPUNIT_ASSERT_EQUAL( 4., q.d());
}

void
QuaternionTest::Constructor3(void)
{
  Quaternion q(90.*constant::math::degToRad, svector(1., -1., .5));

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/2., q.a(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2./9.), q.b(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2./9.), q.c(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(1./18.), q.d(), constant::math::epsilon);
}

void
QuaternionTest::Constructor4(void)
{
  Quaternion q(svector(1., -1., .5));

  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., q.a(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., q.b(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-1, q.c(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(.5, q.d(), constant::math::epsilon);
}

void
QuaternionTest::CopyConstructor(void)
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1(q);

  CPPUNIT_ASSERT_EQUAL( 1., q1.a());
  CPPUNIT_ASSERT_EQUAL( 2., q1.b());
  CPPUNIT_ASSERT_EQUAL( 3., q1.c());
  CPPUNIT_ASSERT_EQUAL( 4., q1.d());
}

void
QuaternionTest::Equal(void)
{
  Quaternion q(1., 2., 3., 4.);
  CPPUNIT_ASSERT_EQUAL(q, q);
}

void
QuaternionTest::Affectation(void)
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  CPPUNIT_ASSERT_EQUAL(q, q1);
}

void
QuaternionTest::PlusEqual(void)
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  q1 += q;
  q += q;

  CPPUNIT_ASSERT_EQUAL(Quaternion(2., 4., 6., 8.), q1);
  CPPUNIT_ASSERT_EQUAL(Quaternion(2., 4., 6., 8.), q);
}

void
QuaternionTest::MinusEqual(void)
{
  Quaternion qref(0., 0., 0., 0.);
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  q1 -= q;
  q -= q;

  CPPUNIT_ASSERT_EQUAL(qref, q1);
  CPPUNIT_ASSERT_EQUAL(qref, q);
}

void
QuaternionTest::TimesEqual(void)
{
  Quaternion qref(-28., 4., 6., 8.);
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  q1 *= q;
  q *= q;

  CPPUNIT_ASSERT_EQUAL(qref, q1);
  CPPUNIT_ASSERT_EQUAL(qref, q);
}

void
QuaternionTest::DivideEqual(void)
{
  Quaternion q(-28., 4., 6., 8.);
  Quaternion q1 = q;

  q1 /= 4.;
  q /= 4.;

  CPPUNIT_ASSERT_EQUAL(Quaternion(-7., 1., 3./2., 2.), q1);
  CPPUNIT_ASSERT_EQUAL(Quaternion(-7., 1., 3./2., 2.), q);
}

void
QuaternionTest::Norm2(void)
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(30.), q.norm2(), constant::math::epsilon);
}

void
QuaternionTest::Conjugate(void)
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_EQUAL(Quaternion(1., -2., -3., -4.), q.conjugate());
}

void
QuaternionTest::DotProduct(void)
{
  Quaternion q1(1., 2., 3., 4.);
  Quaternion q2(5., -6, -3., 2.);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(-8., q1.dotProduct(q2), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30, q1.dotProduct(q1), constant::math::epsilon);
}

void
QuaternionTest::Invert(void)
{
  Quaternion q1(90*constant::math::degToRad, svector(1., -1., .5));

  CPPUNIT_ASSERT_EQUAL( q1.conjugate(), q1.invert() );
}

void
QuaternionTest::AsMatrix(void)
{
  smatrix Mref( 0.,-1., 0.,
                1., 0., 0.,
                0., 0., 1.);

  Quaternion q(90.*constant::math::degToRad, svector(0., 0., 2.));

  CPPUNIT_ASSERT_EQUAL(Mref, q.asMatrix());
}

void
QuaternionTest::getAngleAndAxe(void)
{
  svector axe;
  double angle;

  Quaternion q(45.*constant::math::degToRad, svector(0, 0, 2));
  q.getAngleAndAxe(angle, axe);

  CPPUNIT_ASSERT_EQUAL(svector(0 ,0, 1), axe);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45.*constant::math::degToRad, angle, constant::math::epsilon);
}

void
QuaternionTest::persistanceIO(void)
{
  Quaternion q_ref;
  Quaternion q;
  Quaternion q1_ref(1, 2, 3, 4);
  Quaternion q1;
  std::stringstream flux;

  q_ref.toStream(flux);
  q.fromStream(flux);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);

  q = Quaternion();
  q_ref.toStream(flux);
  q1_ref.toStream(flux);
  q.fromStream(flux);
  q1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(q_ref, q);
  CPPUNIT_ASSERT_EQUAL(q1_ref, q1);
}
