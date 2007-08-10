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
  CPPUNIT_ASSERT_THROW(hkl::Axe("", "", 2, 1, 3), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(hkl::Axe("toto", "", 2, 1, 3), hkl::HKLException);
  // Axe no more check for the validity of the min-max interval
  //CPPUNIT_ASSERT_THROW(hkl::Axe("toto", "titi", 2, 1, 3), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(hkl::Axe("toto", "titi", 1, 2, 3));

  // 1st constructor
  hkl::Axe axe("toto", "titi", 1, 2, 3);
  CPPUNIT_ASSERT_EQUAL(string("toto"), axe.get_name());
  CPPUNIT_ASSERT_EQUAL(string("titi"), axe.get_description());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(1), axe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2), axe.get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(3), axe.get_max());

  // copy constructor
  hkl::Axe axe1(axe);
  CPPUNIT_ASSERT_EQUAL(axe, axe1);
}

void
AxeTest::set(void)
  {
    hkl::Axe axe("toto", "titi", 1, 2, 3);

    axe.set_min(-10);
    axe.set_max(10);
    axe.set_current(5);

    CPPUNIT_ASSERT_EQUAL(std::string("toto"), axe.get_name());
    CPPUNIT_ASSERT_EQUAL(std::string("titi"), axe.get_description());
    CPPUNIT_ASSERT_EQUAL(hkl::Value(-10), axe.get_min());
    CPPUNIT_ASSERT_EQUAL(hkl::Value(5.), axe.get_current());
    CPPUNIT_ASSERT_EQUAL(hkl::Value(10.), axe.get_max());
  }

void
AxeTest::get_distance(void)
{
  hkl::Axe A("toto", "titi", -2*hkl::constant::math::pi, 10 * hkl::constant::math::degToRad, 2*hkl::constant::math::pi);
  hkl::Axe B("toto", "titi", -2*hkl::constant::math::pi, -10 * hkl::constant::math::degToRad, 2*hkl::constant::math::pi);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(90 * hkl::constant::math::degToRad);
  B.set_current(-90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(120 * hkl::constant::math::degToRad);
  B.set_current(-150 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(270 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(-240 * hkl::constant::math::degToRad);
  B.set_current(200 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(440 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(200 * hkl::constant::math::degToRad);
  B.set_current(240 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);

  A.set_current(-90 * hkl::constant::math::degToRad);
  B.set_current(-100 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * hkl::constant::math::degToRad, A.get_distance(B), hkl::constant::math::epsilon);
}

void
AxeTest::persistanceIO(void)
{
  hkl::Axe axe_ref("toto", "titi", -hkl::constant::math::pi, 10 * hkl::constant::math::degToRad, hkl::constant::math::pi);
  hkl::Axe axe("toto", "titi", -hkl::constant::math::pi, -10 * hkl::constant::math::degToRad, hkl::constant::math::pi);

  hkl::Axe axe1_ref("toto", "titi", -hkl::constant::math::pi, 1 * hkl::constant::math::degToRad, hkl::constant::math::pi);
  hkl::Axe axe1("toto", "tutu", -hkl::constant::math::pi, 2 * hkl::constant::math::degToRad, hkl::constant::math::pi);

  stringstream flux;
  axe_ref.toStream(flux);
  axe1_ref.toStream(flux);
  axe.fromStream(flux);
  axe1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(axe_ref, axe);
  CPPUNIT_ASSERT_EQUAL(axe1_ref, axe1);
}
