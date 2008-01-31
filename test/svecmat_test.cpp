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
// File to test matrix and vector implementation.
#include "svecmat_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( vectorMatrixTest );

void
vectorMatrixTest::setUp(void) {}

void
vectorMatrixTest::tearDown(void) {}

void
vectorMatrixTest::SVectorConstructor1(void)
{
  svector v;

  CPPUNIT_ASSERT_EQUAL( 0., v.x());
  CPPUNIT_ASSERT_EQUAL( 0., v.y());
  CPPUNIT_ASSERT_EQUAL( 0., v.z());
}

void
vectorMatrixTest::SVectorConstructor2(void)
{
  svector v(0., 1., 2.);

  CPPUNIT_ASSERT_EQUAL( 0., v.x());
  CPPUNIT_ASSERT_EQUAL( 1., v.y());
  CPPUNIT_ASSERT_EQUAL( 2., v.z());
}

void
vectorMatrixTest::SVectorEqual(void)
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(1.0, 2.0, 3.0);

  CPPUNIT_ASSERT_EQUAL(v1, v1);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(v1,v2));
}

void
vectorMatrixTest::SVectorCopyConstructor(void)
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(v1);
  CPPUNIT_ASSERT_EQUAL(v1, v2);
}

void
vectorMatrixTest::SVectorSet(void)
{
  svector vref(1.0, 2.0, 3.0);
  svector v(5.0, 6.0, 7.0);

  v.set(1.0, 2.0, 3.0);
  CPPUNIT_ASSERT_EQUAL(vref, v);
}

void
vectorMatrixTest::SMatrixConstructor1(void)
{
  smatrix matrice;

  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(0,0));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(0,1));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(0,2));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(1,0));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(1,1));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(1,2));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(2,0));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(2,1));
  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(2,2));
}

void
vectorMatrixTest::SMatrixConstructor2(void)
{
  smatrix matrice(0.0, 1.0, 2.0,
                  3.0, 4.0, 5.0,
                  6.0, 7.0, 8.0);

  CPPUNIT_ASSERT_EQUAL(0.0, matrice.get(0,0));
  CPPUNIT_ASSERT_EQUAL(1.0, matrice.get(0,1));
  CPPUNIT_ASSERT_EQUAL(2.0, matrice.get(0,2));
  CPPUNIT_ASSERT_EQUAL(3.0, matrice.get(1,0));
  CPPUNIT_ASSERT_EQUAL(4.0, matrice.get(1,1));
  CPPUNIT_ASSERT_EQUAL(5.0, matrice.get(1,2));
  CPPUNIT_ASSERT_EQUAL(6.0, matrice.get(2,0));
  CPPUNIT_ASSERT_EQUAL(7.0, matrice.get(2,1));
  CPPUNIT_ASSERT_EQUAL(8.0, matrice.get(2,2));
}

void
vectorMatrixTest::SMatrixConstructor3(void)
{
  smatrix matrix(45.*constant::math::degToRad, 45.*constant::math::degToRad, 45.*constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL( .5, matrix.get(0,0), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-.5, matrix.get(0,1), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1./sqrt(2.), matrix.get(0,2), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/4.+1./2., matrix.get(1,0), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2.)/4.+1./2., matrix.get(1,1), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-.5, matrix.get(1,2), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2.)/4.+1./2., matrix.get(2,0), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/4.+1./2., matrix.get(2,1), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( .5, matrix.get(2,2), constant::math::epsilon);
}

void
vectorMatrixTest::SMatrixEqual(void)
{
  smatrix m1(0.0, 1.0, 2.0,
             3.0, 4.0, 5.0,
             6.0, 7.0, 8.0);

  smatrix m2(1.0, 1.0, 2.0,
             3.0, 4.0, 5.0,
             6.0, 7.0, 8.0);

  CPPUNIT_ASSERT_EQUAL(m1, m1);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(m1, m2));
}

void
vectorMatrixTest::SMatrixCopyConstructor(void)
{
  smatrix m1(0.0, 1.0, 2.0,
             3.0, 4.0, 5.0,
             6.0, 7.0, 8.0);

  smatrix m2(m1);
  CPPUNIT_ASSERT_EQUAL(m1, m2);
}

void
vectorMatrixTest::Norm2(void)
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(-1.0, 1.0, 2.0);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(5.0), v1.norm2(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(6.0), v2.norm2(), constant::math::epsilon);
}

void
vectorMatrixTest::Normalize(void)
{
  svector v1(1. /sqrt(2.), 1. / sqrt(2.), 0.);
  svector v(1., 1., 0.);

  CPPUNIT_ASSERT_EQUAL(v1, v.normalize());
}

void
vectorMatrixTest::Scalar(void)
{
  svector v1(0.0, 1.0, 2.0);

  svector v(v1);

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, v.scalar(v1), constant::math::epsilon );
}

void
vectorMatrixTest::VectorialProduct(void)
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(1.0, 2.0, 3.0);
  svector vref(-1.0, 2.0, -1.0);

  CPPUNIT_ASSERT_EQUAL( vref, v1.vectorialProduct(v2) );
}

void
vectorMatrixTest::Angle(void)
{
  double angle;
  svector v(1., 0., 0.);
  svector v1(1., 1., .5);

  angle = v.angle(svector(1., 0., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, constant::math::epsilon);

  angle = v.angle(svector(1., 1., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, constant::math::epsilon);

  angle = v1.angle(svector(1, .5, -1.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, constant::math::epsilon);
}

void
vectorMatrixTest::AxisSystem(void)
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(1.0, 2.0, 3.0);
  svector v3(-1.0, 2.0, -1.0);
  smatrix mref(0.0,             5.0 / sqrt(30.0), -1.0 / sqrt(6.0),
               1.0 / sqrt(5.0), 2.0 / sqrt(30.0),  2.0 / sqrt(6.0),
               2.0 / sqrt(5.0),-1.0 / sqrt(30.0), -1.0 / sqrt(6.0));

  CPPUNIT_ASSERT_EQUAL(mref, v1.axisSystem(v2));
}

void
vectorMatrixTest::rotatedAroundVector(void)
{
  svector x(1, 0, 0);
  svector z(0, 0, 1);
  svector y(0, 1, 0);

  CPPUNIT_ASSERT_EQUAL(y, x.rotatedAroundVector(z, 90*constant::math::degToRad));
}

void
vectorMatrixTest::svector_TimesEqual_smatrix(void)
{
  smatrix m( 1.0, 3.0, -2.0,
             10.0, 5.0, 5.0,
             -3.0, 2.0, 0.0);

  svector v(1.0, 2.0, 3.0);
  v *= m;
  CPPUNIT_ASSERT_EQUAL(svector(12., 19., 8.), v);
}

void
vectorMatrixTest::smatrix_Times_svector(void)
{
  smatrix m( 1.0, 3.0, -2.0,
             10.0, 5.0, 5.0,
             -3.0, 2.0, 0.0);

  svector v = m * svector(1.0, 2.0, 3.0);

  CPPUNIT_ASSERT_EQUAL(svector(1., 35., 1.), v);
}

void
vectorMatrixTest::smatrix_TimesEqual_smatrix(void)
{
  smatrix Mref(37., 14., 13.,
               45., 65.,  5.,
               17.,  1., 16.);

  smatrix M( 1., 3.,-2.,
             10., 5., 5.,
             -3., 2., 0.);

  M *= M;
  CPPUNIT_ASSERT_EQUAL(Mref, M);
}

void
vectorMatrixTest::smatrix_Times_smatrix(void)
{
  smatrix Mref(37., 14., 13.,
               45., 65.,  5.,
               17.,  1., 16.);

  smatrix M( 1., 3.,-2.,
             10., 5., 5.,
             -3., 2., 0.);

  M = M * M;
  CPPUNIT_ASSERT_EQUAL(Mref, M);
}

void
vectorMatrixTest::AsEulerian(void)
{
  smatrix M(             1./2.,             -1./2., sqrt(2)/2.,
                         sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.,
                         -sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.);
  svector vref(45.*constant::math::degToRad, 45.*constant::math::degToRad, 45.*constant::math::degToRad);

  svector v = M.asEulerian();
  CPPUNIT_ASSERT_EQUAL(vref, v);
}

void
vectorMatrixTest::svector_IO(void)
{
  svector v1_ref(1, 2, 3);
  svector v2_ref(4, 5, 6);
  svector v1;
  svector v2;
  std::stringstream flux;

  v1_ref.toStream(flux);
  v1.fromStream(flux);
  CPPUNIT_ASSERT_EQUAL(v1_ref, v1);

  v1.set(0,0,0);
  v1_ref.toStream(flux);
  v2_ref.toStream(flux);
  v1.fromStream(flux);
  v2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(v1_ref, v1);
  CPPUNIT_ASSERT_EQUAL(v2_ref, v2);
}

void
vectorMatrixTest::smatrix_IO(void)
{
  smatrix M1_ref(1, 2, 3, 4, 5, 6, 7, 8, 9);
  smatrix M1;
  smatrix M2_ref(10, 11, 12, 13, 14, 15, 16, 17, 18);
  smatrix M2;
  std::stringstream flux;

  M1_ref.toStream(flux);
  M1.fromStream(flux);
  CPPUNIT_ASSERT_EQUAL(M1_ref, M1);

  M1.set(0,0,0,0,0,0,0,0,0);
  M1_ref.toStream(flux);
  M2_ref.toStream(flux);
  M1.fromStream(flux);
  M2.fromStream(flux);
  CPPUNIT_ASSERT_EQUAL(M1_ref, M1);
  CPPUNIT_ASSERT_EQUAL(M2_ref, M2);
}
