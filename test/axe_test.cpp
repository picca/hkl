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
  Axe A("toto", svector(0., 0., 1.), 1);

  CPPUNIT_ASSERT_EQUAL(MyString("toto"), A.get_name());
  CPPUNIT_ASSERT_EQUAL(-constant::math::pi, A.get_min());
  CPPUNIT_ASSERT_EQUAL(0., A.get_value());
  CPPUNIT_ASSERT_EQUAL(constant::math::pi, A.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 1.), A.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, A.get_direction());

  double position = 12 * constant::math::degToRad;
  Axe B("toto", svector(0., 0., 1.), 1, position);

  CPPUNIT_ASSERT_EQUAL(MyString("toto"), B.get_name());
  CPPUNIT_ASSERT_EQUAL(-constant::math::pi, B.get_min());
  CPPUNIT_ASSERT_EQUAL(position, B.get_value());
  CPPUNIT_ASSERT_EQUAL(constant::math::pi, B.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 1.), B.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, B.get_direction());
}

void
AxeTest::equal(void)
{
  Axe A("toto", svector(0., 0., 1.), 1);
  Axe B("toto", svector(0., 0., 1.), 1);

  CPPUNIT_ASSERT_EQUAL(A, A);
  CPPUNIT_ASSERT_EQUAL(A, B);
}

void
AxeTest::set(void)
{
  Axe A("toto", svector(0., 0., 1.), 1);

  A.set_name("titi");
  A.set_min(-10.);
  A.set_value(5);
  A.set_max(10.);
  A.set_axe(svector(5., -3., 10.));
  A.set_direction(10);

  CPPUNIT_ASSERT_EQUAL(MyString("titi"), A.get_name());
  CPPUNIT_ASSERT_EQUAL(-10., A.get_min());
  CPPUNIT_ASSERT_EQUAL(5., A.get_value());
  CPPUNIT_ASSERT_EQUAL(10., A.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(5., -3., 10.), A.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, A.get_direction());
}

void
AxeTest::asQuaternion(void)
{
  Axe  A("toto", svector(0., 0., 1.), 1);
  Quaternion q(90. * constant::math::degToRad, svector(0., 0., 1.));
  
  A.set_value(90. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(q, A.asQuaternion());
}

void
AxeTest::getDistance(void)
{
  Axe A("toto", svector(0., 0., 1.), 1, 10 * constant::math::degToRad);
  Axe B("toto", svector(0., 0., 1.), 1, -10 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);

  A.set_value(90 * constant::math::degToRad);
  B.set_value(-90 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);

  A.set_value(120 * constant::math::degToRad);
  B.set_value(-150 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);

  A.set_value(-240 * constant::math::degToRad);
  B.set_value(200 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);

  A.set_value(200 * constant::math::degToRad);
  B.set_value(240 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);

  A.set_value(-90 * constant::math::degToRad);
  B.set_value(-100 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad, A.getDistance(B), constant::math::epsilon_0);
}

void
AxeTest::persistanceIO(void)
{
  Axe axe_ref("\n", svector(1e-8, 2, -3e14), -1); 
  Axe axe;
  
  Axe axe1_ref("l'axe de maman", svector(2, 2, 0), -1);
  Axe axe1;
  
  stringstream flux;
  axe_ref.toStream(flux);
  axe1_ref.toStream(flux);
  axe.fromStream(flux);
  axe1.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(axe_ref, axe);
  CPPUNIT_ASSERT_EQUAL(axe1_ref, axe1);
}
