// File to test quaternion implementation.
#include "axe_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( AxeTest );

void
AxeTest::setUp(void)
{}

void
AxeTest::tearDown(void)
{}

void
AxeTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(Axe("", "", 2, 1, 3, svector(0., 0., 0), 0), HKLException);
  CPPUNIT_ASSERT_THROW(Axe("toto", "", 2, 1, 3, svector(0., 0., 0), 0), HKLException);
  CPPUNIT_ASSERT_THROW(Axe("toto", "titi", 1, 2, 3, svector(0., 0., 0), 0), HKLException);
  CPPUNIT_ASSERT_THROW(Axe("toto", "titi", 1, 2, 3, svector(0., 0., 1), 0), HKLException);
  CPPUNIT_ASSERT_NO_THROW(Axe("toto", "titi", 1, 2, 3, svector(0., 0., 1), 1));

  // 1st constructor
  Axe axe("toto", "titi", 1, 2, 3, svector(0., 0., 1), 1);
  CPPUNIT_ASSERT_EQUAL(MyString("toto"), axe.get_name());
  CPPUNIT_ASSERT_EQUAL(MyString("titi"), axe.get_description());
  CPPUNIT_ASSERT_EQUAL(Value(1), axe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(Value(3), axe.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 1.), axe.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, axe.get_direction());

  // copy constructor
  Axe axe1(axe);
  CPPUNIT_ASSERT_EQUAL(axe, axe1);
}

void
AxeTest::set(void)
  {
    Axe axe("toto", "titi", 1, 2, 3, svector(0., 0., 1), 1);

    axe.set_range(-10., 10);
    axe.set_current(5);

    CPPUNIT_ASSERT_EQUAL(MyString("toto"), axe.get_name());
    CPPUNIT_ASSERT_EQUAL(MyString("titi"), axe.get_description());
    CPPUNIT_ASSERT_EQUAL(Value(-10), axe.get_min());
    CPPUNIT_ASSERT_EQUAL(Value(5.), axe.get_current());
    CPPUNIT_ASSERT_EQUAL(Value(10.), axe.get_max());
    CPPUNIT_ASSERT_EQUAL(svector(0, 0, 1), axe.get_axe());
    CPPUNIT_ASSERT_EQUAL(1, axe.get_direction());
  }

void
AxeTest::asQuaternion(void)
{
  Axe  axe("toto", "titi", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1);
  Quaternion q(90. * constant::math::degToRad, svector(0., 0., 1.));

  axe.set_current(90. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(q, axe.asQuaternion());
}

void
AxeTest::getDistance(void)
{
  Axe A("toto", "titi", -2*constant::math::pi, 10 * constant::math::degToRad, 2*constant::math::pi, svector(0., 0., 1.), 1);
  Axe B("toto", "titi", -2*constant::math::pi, -10 * constant::math::degToRad, 2*constant::math::pi, svector(0., 0., 1.), 1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);

  A.set_current(90 * constant::math::degToRad);
  B.set_current(-90 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);

  A.set_current(120 * constant::math::degToRad);
  B.set_current(-150 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);

  A.set_current(-240 * constant::math::degToRad);
  B.set_current(200 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);

  A.set_current(200 * constant::math::degToRad);
  B.set_current(240 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);

  A.set_current(-90 * constant::math::degToRad);
  B.set_current(-100 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon);
}

void
AxeTest::persistanceIO(void)
{
  Axe axe_ref("toto", "titi", -constant::math::pi, 10 * constant::math::degToRad, constant::math::pi, svector(0, 0, 1), 1);
  Axe axe("toto", "titi", -constant::math::pi, -10 * constant::math::degToRad, constant::math::pi, svector(1, 0, 1), 1);

  Axe axe1_ref("toto", "titi", -constant::math::pi, 1 * constant::math::degToRad, constant::math::pi, svector(1, 0, 1), 1);
  Axe axe1("toto", "tutu", -constant::math::pi, 2 * constant::math::degToRad, constant::math::pi, svector(1, 0, 1), 1);

  stringstream flux;
  axe_ref.toStream(flux);
  axe1_ref.toStream(flux);
  axe.fromStream(flux);
  axe1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(axe_ref, axe);
  CPPUNIT_ASSERT_EQUAL(axe1_ref, axe1);
}
