// File to test quaternion implementation.
#include <cmath>

#include "config.h"
#include "axe_rotation_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( AxeRotationTest );

void
AxeRotationTest::setUp(void) {}

void
AxeRotationTest::tearDown(void) {}

void
AxeRotationTest::constructors(void)
{
  static hkl_svector svector_Z = {{0, 0, 1}};
  hkl_quaternion q_ref;
  ::hkl_quaternion_from_angle_and_axe(&q_ref, 2, &svector_Z);

  //CPPUNIT_ASSERT_THROW(hkl::axe::Rotation("toto", "titi", 1, 2, 3, &hkl_svector_null), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(hkl::axe::Rotation("toto", "titi", 1, 2, 3, &svector_Z));

  // 1st constructor
  hkl::axe::Rotation rotation("toto", "titi", 1, 2, 3, &svector_Z);
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_svector_cmp(&svector_Z, rotation.get_axe()));
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_ref, rotation.get_quaternion()));
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_ref, rotation.get_quaternion_consign()));

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
  hkl_svector axe_ref = {{0., 0., 1}};
  hkl_quaternion q_ref;
  hkl_quaternion q_consign_ref;
  ::hkl_quaternion_from_angle_and_axe(&q_ref, 2.5, &axe_ref);
  ::hkl_quaternion_from_angle_and_axe(&q_consign_ref, 2, &axe_ref);

  hkl::axe::Rotation axe("toto", "titi", 1, 2, 3, &axe_ref);

  // set the current value of the axe
  axe.set_current(2.5);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.), axe.get_consign());
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_ref, axe.get_quaternion()));
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_consign_ref, axe.get_quaternion_consign()));

  // set the consign of the axe.
  axe.set_consign(2.5);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2.5), axe.get_consign());
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_ref, axe.get_quaternion()));
  CPPUNIT_ASSERT_EQUAL(1, ::hkl_quaternion_cmp(&q_ref, axe.get_quaternion_consign()));
}

void
AxeRotationTest::get_distance(void)
{
  hkl_svector axe = {{0, 0, 1}};
  hkl::axe::Rotation A("toto", "titi", -2*M_PI, 10 * HKL_DEGTORAD, 2*M_PI, &axe);
  hkl::axe::Rotation B("toto", "titi", -2*M_PI, -10 * HKL_DEGTORAD, 2*M_PI, &axe);

  // get_distance
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);

  A.set_current(90 * HKL_DEGTORAD);
  B.set_current(-90 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_current(120 * HKL_DEGTORAD);
  B.set_current(-150 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_current(-240 * HKL_DEGTORAD);
  B.set_current(200 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_current(200 * HKL_DEGTORAD);
  B.set_current(240 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_current(-90 * HKL_DEGTORAD);
  B.set_current(-100 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  // get_distance_consign
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_consign(90 * HKL_DEGTORAD);
  B.set_consign(-90 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_consign(120 * HKL_DEGTORAD);
  B.set_consign(-150 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_consign(-240 * HKL_DEGTORAD);
  B.set_consign(200 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_consign(200 * HKL_DEGTORAD);
  B.set_consign(240 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

  A.set_consign(-90 * HKL_DEGTORAD);
  B.set_consign(-100 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);
}
