// File to test quaternion implementation.
#include "quaternion_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( QuaternionTest );

void
QuaternionTest::setUp(void) {}

void
QuaternionTest::tearDown(void) {}

void
QuaternionTest::hkl_quaternion_cmp(void)
{
  hkl_quaternion q_ref = {{1., 2., 3., 4.}};
  hkl_quaternion q = {{1., 2., 3., 4.}};
  hkl_quaternion q1 = {{1., 1., 3., 4.}};
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));

  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_quaternion_cmp(&q_ref, &q1));
  q1 = q_ref;
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q1));
}

void
QuaternionTest::hkl_quaternion_from_svector(void)
{
  hkl_quaternion q_ref = {{0, 1, -1, .5}};
  hkl_svector v = {{1., -1., .5}};
  hkl_quaternion q;

  ::hkl_quaternion_from_svector(&q, &v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_from_angle_and_axe(void)
{
  hkl_quaternion q_ref = {{sqrt(2.)/2., sqrt(2./9.), -sqrt(2./9.), sqrt(1./18.)}};
  hkl_svector v = {{1., -1., .5}};
  hkl_quaternion q;

  ::hkl_quaternion_from_angle_and_axe(&q, 90. * HKL_DEGTORAD, &v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_times_quaternion(void)
{
  hkl_quaternion q_ref = {{-28., 4., 6., 8.}};
  hkl_quaternion q = {{1., 2., 3., 4.}};

  ::hkl_quaternion_times_quaternion(&q, &q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_norm2(void)
{
  hkl_quaternion q = {{1., 2., 3., 4.}};

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(30.), ::hkl_quaternion_norm2(&q), HKL_EPSILON);
}

void
QuaternionTest::hkl_quaternion_conjugate(void)
{
  hkl_quaternion q_ref = {{1., -2., -3., -4.}};
  hkl_quaternion q = {{1., 2., 3., 4.}};

  ::hkl_quaternion_conjugate(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_to_smatrix(void)
{
  hkl_smatrix m_ref = {{{0.,-1., 0.},
      {1., 0., 0.},
      {0., 0., 1.}}
  };
  hkl_smatrix m;
  hkl_quaternion q_ref;
  hkl_svector v_ref = {{0., 0., 2.}};

  ::hkl_quaternion_from_angle_and_axe(&q_ref, 90.*HKL_DEGTORAD, &v_ref);
  ::hkl_quaternion_to_smatrix(&q_ref, &m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
QuaternionTest::hkl_quaternion_to_angle_and_axe(void)
{
  hkl_svector v_ref = {{0 ,0, 1}};
  hkl_quaternion q;
  double angle;
  hkl_svector v;

  ::hkl_quaternion_from_angle_and_axe(&q, 45.*HKL_DEGTORAD, &v_ref);
  ::hkl_quaternion_to_angle_and_axe(&q, &angle, &v);

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_ref, &v));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45.*HKL_DEGTORAD, angle, HKL_EPSILON);
}
