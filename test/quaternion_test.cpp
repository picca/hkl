// File to test quaternion implementation.
#include <cmath>

#include "quaternion_test.h"
#include "config.h"
#include "svector.h"
#include "smatrix.h"

CPPUNIT_TEST_SUITE_REGISTRATION( QuaternionTest );

void
QuaternionTest::setUp(void) {}

void
QuaternionTest::tearDown(void) {}

void
QuaternionTest::hkl_quaternion_cmp(void)
{
  static hkl_quaternion q_ref = {{1., 2., 3., 4.}};
  static hkl_quaternion q = {{1., 2., 3., 4.}};
  hkl_quaternion q1 = {{1., 1., 3., 4.}};
  
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_quaternion_cmp(&q_ref, &q1));
  
  // test the assignation
  q1 = q_ref;
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q1));
}

void
QuaternionTest::hkl_quaternion_from_svector(void)
{
  static hkl_quaternion q_ref = {{0, 1, -1, .5}};
  static hkl_svector v = {{1., -1., .5}};
  hkl_quaternion q;

  ::hkl_quaternion_from_svector(&q, &v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_from_angle_and_axe(void)
{
  static hkl_quaternion q_ref1 = {{1, 0, 0, 0}};
  static hkl_quaternion q_ref2 = {{sqrt(2.)/2., sqrt(2./9.), -sqrt(2./9.), sqrt(1./18.)}};
  static hkl_svector v_ref2 = {{1., -1., .5}};
  hkl_quaternion q;

  ::hkl_quaternion_from_angle_and_axe(&q, 0, &v_ref2);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref1, &q));

  ::hkl_quaternion_from_angle_and_axe(&q, 90. * HKL_DEGTORAD, &v_ref2);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref2, &q));
}

void
QuaternionTest::hkl_quaternion_times_quaternion(void)
{
  static hkl_quaternion q_ref = {{-28., 4., 6., 8.}};
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
  static hkl_quaternion q_ref = {{1., -2., -3., -4.}};
  hkl_quaternion q = {{1., 2., 3., 4.}};

  ::hkl_quaternion_conjugate(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
QuaternionTest::hkl_quaternion_to_smatrix(void)
{
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
  static hkl_smatrix m_ref = {{{0,-1, 0},
                               {1, 0, 0},
                               {0, 0, 1}}};
  hkl_smatrix m;

  ::hkl_quaternion_to_smatrix(&q_ref, &m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
QuaternionTest::hkl_quaternion_to_angle_and_axe(void)
{
  static hkl_svector v_ref = {{0 ,0, 1}};
  static hkl_svector v_null = {{0 ,0, 0}};
  static hkl_quaternion q_I = {{1, 0, 0, 0}};

  int i;
  double angle_ref;
  double angle;
  hkl_svector v;
  hkl_quaternion q;

  
  // test the q = (1, 0, 0, 0) solution axe == (0, 0, 0) and angle = 0.
  ::hkl_quaternion_to_angle_and_axe(&q_I, &angle, &v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_null, &v));
  CPPUNIT_ASSERT_EQUAL(0., angle);

  // test other cases
  for(i=-180; i<180; i++)
  {
    angle_ref = i *  HKL_DEGTORAD;
    ::hkl_quaternion_from_angle_and_axe(&q, angle_ref, &v_ref);
    ::hkl_quaternion_to_angle_and_axe(&q, &angle, &v);
    
    if (::hkl_svector_cmp(&v_ref, &v))
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle_ref, angle, HKL_EPSILON);
    else if (::hkl_svector_is_opposite(&v, &v_ref))
      CPPUNIT_ASSERT_DOUBLES_EQUAL(angle_ref, -angle, HKL_EPSILON);
  }
}
