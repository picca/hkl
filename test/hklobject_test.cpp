#include "hklobject_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( HKLObjectTest );

void
HKLObjectTest::setUp(void) {}

void
HKLObjectTest::tearDown(void) {}

void
HKLObjectTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(HKLObject("", ""), HKLException);
  CPPUNIT_ASSERT_THROW(HKLObject("titi", ""), HKLException);
  CPPUNIT_ASSERT_NO_THROW(HKLObject("titi", "toto"));

  // 1st constructor
  HKLObject hklObject("titi", "toto");
  CPPUNIT_ASSERT_EQUAL(std::string("titi"), hklObject.get_name());
  CPPUNIT_ASSERT_EQUAL(std::string("toto"), hklObject.get_description());

  // copy constructor
  HKLObject hklObject1(hklObject);

  CPPUNIT_ASSERT_EQUAL(hklObject, hklObject1);
}
