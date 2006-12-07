// File to test quaternion implementation.
#include "quaternion_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( QuaternionTest );

void
QuaternionTest::setUp(void)
{}

void
QuaternionTest::tearDown(void)
{}

void
QuaternionTest::Constructor1(void)
{
  Quaternion q;

  CPPUNIT_ASSERT_EQUAL( 1., q[0]);
  CPPUNIT_ASSERT_EQUAL( 0., q[1]);
  CPPUNIT_ASSERT_EQUAL( 0., q[2]);
  CPPUNIT_ASSERT_EQUAL( 0., q[3]);
}

void
QuaternionTest::Constructor2(void)
{
  Quaternion q(1., 2., 3., 4.);

  CPPUNIT_ASSERT_EQUAL( 1., q[0]);
  CPPUNIT_ASSERT_EQUAL( 2., q[1]);
  CPPUNIT_ASSERT_EQUAL( 3., q[2]);
  CPPUNIT_ASSERT_EQUAL( 4., q[3]);
}

void
QuaternionTest::Constructor3(void)
{
  Quaternion q(90.*constant::math::degToRad, svector(1., -1., .5));

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2.)/2., q[0], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(2./9.), q[1], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-sqrt(2./9.), q[2], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt(1./18.), q[3], constant::math::epsilon_1);
}

void
QuaternionTest::Constructor4(void)
{
  Quaternion q(svector(1., -1., .5));

  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., q[0], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., q[1], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-1, q[2], constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(.5, q[3], constant::math::epsilon_1);
}

void
QuaternionTest::CopyConstructor(void)
{
  Quaternion q(1., 2., 3., 4.);
  Quaternion q1(q);

  CPPUNIT_ASSERT_EQUAL( 1., q1[0]);
  CPPUNIT_ASSERT_EQUAL( 2., q1[1]);
  CPPUNIT_ASSERT_EQUAL( 3., q1[2]);
  CPPUNIT_ASSERT_EQUAL( 4., q1[3]);
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

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(30.), q.norm2(), constant::math::epsilon_1);
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

  CPPUNIT_ASSERT_DOUBLES_EQUAL(-8., q1.dotProduct(q2), constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30, q1.dotProduct(q1), constant::math::epsilon_1);
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
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45.*constant::math::degToRad, angle, constant::math::epsilon_1);
}

void
QuaternionTest::persistanceIO(void)
{
  Quaternion q_ref;
  Quaternion q;
  Quaternion q1_ref(1, 2, 3, 4);
  Quaternion q1;
  stringstream flux;

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
