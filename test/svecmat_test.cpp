// File to test matrix and vector implementation.
#include "svecmat_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( vectorMatrixTest );

void
vectorMatrixTest::setUp()
{}

void 
vectorMatrixTest::tearDown() 
{
}

void 
vectorMatrixTest::SVectorConstructor1()
{
  svector v;

  CPPUNIT_ASSERT_EQUAL( 0., v[X]);
  CPPUNIT_ASSERT_EQUAL( 0., v[Y]);
  CPPUNIT_ASSERT_EQUAL( 0., v[Z]);
}

void
vectorMatrixTest::SVectorConstructor2()
{
  svector v(0., 1., 2.);
  
  CPPUNIT_ASSERT_EQUAL( 0., v[X]);
  CPPUNIT_ASSERT_EQUAL( 1., v[Y]);
  CPPUNIT_ASSERT_EQUAL( 2., v[Z]);
}

void
vectorMatrixTest::SVectorEqual()
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(1.0, 2.0, 3.0);

  CPPUNIT_ASSERT_EQUAL(v1, v1);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(v1,v2));
}

void
vectorMatrixTest::SVectorCopyConstructor()
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(v1);
  CPPUNIT_ASSERT_EQUAL(v1, v2);
}

void
vectorMatrixTest::SVectorSet()
{
  svector vref(1.0, 2.0, 3.0);
  svector v(5.0, 6.0, 7.0);
  
  v.set(1.0, 2.0, 3.0);
  CPPUNIT_ASSERT_EQUAL(vref, v);
}

void
vectorMatrixTest::SMatrixConstructor1()
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
vectorMatrixTest::SMatrixConstructor2()
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
vectorMatrixTest::SMatrixConstructor3()
{
  smatrix matrix(45.*constant::math::degToRad, 45.*constant::math::degToRad, 45.*constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL( .5, matrix.get(0,0), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-.5, matrix.get(0,1), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1./sqrt(2.), matrix.get(0,2), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/4.+1./2., matrix.get(1,0), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2.)/4.+1./2., matrix.get(1,1), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-.5, matrix.get(1,2), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2.)/4.+1./2., matrix.get(2,0), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/4.+1./2., matrix.get(2,1), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( .5, matrix.get(2,2), constant::math::epsilon_1);
}

void
vectorMatrixTest::SMatrixEqual()
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
vectorMatrixTest::SMatrixCopyConstructor()
{
  smatrix m1(0.0, 1.0, 2.0, 
            3.0, 4.0, 5.0,
            6.0, 7.0, 8.0);
  
  smatrix m2(m1);
  CPPUNIT_ASSERT_EQUAL(m1, m2);
}

void
vectorMatrixTest::Norm2()
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(-1.0, 1.0, 2.0);
    
  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(5.0), v1.norm2(), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(6.0), v2.norm2(), constant::math::epsilon_1);
}

void
vectorMatrixTest::NormInf()
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(-6.0, 1.0, 2.0);
    
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2.0, v1.norminf(), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(6.0, v2.norminf(), constant::math::epsilon_1);
}

void
vectorMatrixTest::Normalize()
{
  svector v1(1. /sqrt(2.), 1. / sqrt(2.), 0.);
  svector v(1., 1., 0.);
  
  CPPUNIT_ASSERT_EQUAL(v1, v.normalize());
}

void
vectorMatrixTest::Scalar()
{
  svector v1(0.0, 1.0, 2.0);

  svector v(v1);

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, v.scalar(v1), constant::math::epsilon_1 );
}

void
vectorMatrixTest::VectorialProduct()
{
  svector v1(0.0, 1.0, 2.0);
  svector v2(1.0, 2.0, 3.0);
  svector vref(-1.0, 2.0, -1.0);
  
  CPPUNIT_ASSERT_EQUAL( vref, v1.vectorialProduct(v2) );
}

void 
vectorMatrixTest::Angle()
{ 
  double angle;
  svector v(1., 0., 0.);
  svector v1(1., 1., .5);  

  angle = v.angle(svector(1., 0., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, constant::math::epsilon_1);
  
  angle = v.angle(svector(1., 1., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, constant::math::epsilon_1);
  
  angle = v1.angle(svector(1, .5, -1.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, constant::math::epsilon_1);
}

void
vectorMatrixTest::AxisSystem()
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
vectorMatrixTest::svector_TimesEqual_smatrix()
{
  smatrix m( 1.0, 3.0, -2.0,
            10.0, 5.0, 5.0,
            -3.0, 2.0, 0.0);
  
  svector v(1.0, 2.0, 3.0);
  v *= m;
  CPPUNIT_ASSERT_EQUAL(svector(12., 19., 8.), v);
}

void
vectorMatrixTest::smatrix_Times_svector()
{
  smatrix m( 1.0, 3.0, -2.0,
            10.0, 5.0, 5.0,
            -3.0, 2.0, 0.0);

  svector v = m * svector(1.0, 2.0, 3.0);
  
  CPPUNIT_ASSERT_EQUAL(svector(1., 35., 1.), v);
}

void
vectorMatrixTest::smatrix_TimesEqual_smatrix()
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
vectorMatrixTest::smatrix_Times_smatrix()
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
vectorMatrixTest::AsEulerian()
{
  smatrix M(             1./2.,             -1./2., sqrt(2)/2.,
             sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.,
            -sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.);
  svector vref(45.*constant::math::degToRad, 45.*constant::math::degToRad, 45.*constant::math::degToRad);
  
  svector v = M.asEulerian();
  CPPUNIT_ASSERT_EQUAL(vref, v);
}
