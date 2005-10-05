// File to test quaternion implementation.
#include "axe_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( axeTest );

void
axeTest::setUp()
{}

void 
axeTest::tearDown() 
{}

void 
axeTest::testConstructeur1()
{
  Axe  A("toto", svector(0., 0., 1.), 1);

  CPPUNIT_ASSERT_EQUAL(std::string("toto"), A.get_name());
  CPPUNIT_ASSERT_EQUAL(1., A.get_min());
  CPPUNIT_ASSERT_EQUAL(0., A.get_value());
  CPPUNIT_ASSERT_EQUAL(-1., A.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 1.), A.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, A.get_direction());
}

void
axeTest::testEqual()
{
  Axe A("toto", svector(0., 0., 1.), 1);
  Axe B("toto", svector(0., 0., 1.), 1);

  CPPUNIT_ASSERT_EQUAL(A, A);
  CPPUNIT_ASSERT_EQUAL(A, B);
}

void
axeTest::testSet()
{
  Axe A("toto", svector(0., 0., 1.), 1);

  A.set_name("titi");
  A.set_min(-10.);
  A.set_value(5);
  A.set_max(10.);
  A.set_axe(svector(5., -3., 10.));
  A.set_direction(10);

  CPPUNIT_ASSERT_EQUAL(std::string("titi"), A.get_name());
  CPPUNIT_ASSERT_EQUAL(-10., A.get_min());
  CPPUNIT_ASSERT_EQUAL(5., A.get_value());
  CPPUNIT_ASSERT_EQUAL(10., A.get_max());
  CPPUNIT_ASSERT_EQUAL(svector(5., -3., 10.), A.get_axe());
  CPPUNIT_ASSERT_EQUAL(1, A.get_direction());
}

void
axeTest::testAsQuaternion()
{
  Axe  A("toto", svector(0., 0., 1.), 1);
  Quaternion q(90. * constant::math::degToRad, svector(0., 0., 1.));
  
  A.set_value(90. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(q, A.asQuaternion());
}
