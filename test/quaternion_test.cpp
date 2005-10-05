// File to test quaternion implementation.
#include "quaternion_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( quaternionTest );

void
quaternionTest::setUp()
{}

void 
quaternionTest::tearDown() 
{}

void 
quaternionTest::testConstructor1()
{
  Quaternion q;

  CPPUNIT_ASSERT_EQUAL( 1., q[0]);
  CPPUNIT_ASSERT_EQUAL( 0., q[1]);
  CPPUNIT_ASSERT_EQUAL( 0., q[2]);
  CPPUNIT_ASSERT_EQUAL( 0., q[3]);
}

void 
quaternionTest::testConstructor2()
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_EQUAL( 1., q[0]);
  CPPUNIT_ASSERT_EQUAL( 2., q[1]);
  CPPUNIT_ASSERT_EQUAL( 3., q[2]);
  CPPUNIT_ASSERT_EQUAL( 4., q[3]);
}

void 
quaternionTest::testConstructor3()
{
  Quaternion q(90.*constant::math::degToRad, svector(1., -1., .5));

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/2., q[0], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2./9.), q[1], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2./9.), q[2], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(1./18.), q[3], constant::math::epsilon_1);
}

void 
quaternionTest::testCopyConstructor()
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1(q);

  CPPUNIT_ASSERT_EQUAL( 1., q1[0]);
  CPPUNIT_ASSERT_EQUAL( 2., q1[1]);
  CPPUNIT_ASSERT_EQUAL( 3., q1[2]);
  CPPUNIT_ASSERT_EQUAL( 4., q1[3]);
}

void
quaternionTest::testEqual()
{
  Quaternion q(1., 2., 3., 4.);
  CPPUNIT_ASSERT_EQUAL(q, q);
}

void
quaternionTest::testAffectation()
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  CPPUNIT_ASSERT_EQUAL(q, q1);
}

void
quaternionTest::testPlusEqual()
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1 = q;
  q1 += q;
  q += q;
  
  CPPUNIT_ASSERT_EQUAL(Quaternion(2., 4., 6., 8.), q1);
  CPPUNIT_ASSERT_EQUAL(Quaternion(2., 4., 6., 8.), q);
}

void
quaternionTest::testMinusEqual()
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
quaternionTest::testTimesEqual()
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
quaternionTest::testDivideEqual()
{
  Quaternion q(-28., 4., 6., 8.);
  Quaternion q1 = q;

  q1 /= 4.;
  q /= 4.;
  
  CPPUNIT_ASSERT_EQUAL(Quaternion(-7., 1., 3./2., 2.), q1);
  CPPUNIT_ASSERT_EQUAL(Quaternion(-7., 1., 3./2., 2.), q);
}

void
quaternionTest::testNorm2()
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(30.), q.norm2(), constant::math::epsilon_1);
}

void
quaternionTest::testConjugate()
{
  Quaternion q(1., 2., 3., 4.);
  
  CPPUNIT_ASSERT_EQUAL(Quaternion(1., -2., -3., -4.), q.conjugate());
}

void
quaternionTest::testDotProduct()
{
  Quaternion q1(1., 2., 3., 4.);
  Quaternion q2(5., -6, -3., 2.);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(-8., q1.dotProduct(q2), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30, q1.dotProduct(q1), constant::math::epsilon_1);
}

void
quaternionTest::testInvert()
{
  Quaternion q1(90*constant::math::degToRad, svector(1., -1., .5));

  CPPUNIT_ASSERT_EQUAL( q1.conjugate(), q1.invert() );
}

void
quaternionTest::testAsMatrix()
{
  smatrix Mref( 0.,-1., 0.,
                1., 0., 0.,
                0., 0., 1.);

  Quaternion q(90.*constant::math::degToRad, svector(0., 0., 2.));

  CPPUNIT_ASSERT_EQUAL(Mref, q.asMatrix());
}
