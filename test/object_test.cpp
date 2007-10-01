#include "object_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( objectTest );

void
objectTest::setUp(void)
{
  m_object = Object("object", "nouvel object");
}

void
objectTest::tearDown(void) {}

void
objectTest::Constructor(void)
{
  // throw an exception if the name and description are not ok.
  CPPUNIT_ASSERT_THROW(Object("", ""), HKLException);
  CPPUNIT_ASSERT_THROW(Object("toto", ""), HKLException);
  CPPUNIT_ASSERT_THROW(Object("", "la c'est bon"), HKLException);

  //do not throw exception if name and description are ok.
  CPPUNIT_ASSERT_NO_THROW(Object("un nom", "une description"));

  Object object2("object", "nouvel object");
  CPPUNIT_ASSERT_EQUAL(std::string("nouvel object"), object2.get_description());
}

void
objectTest::Equal(void)
{
  Object object("object", "nouvel object");

  CPPUNIT_ASSERT_EQUAL(m_object, object);
}

void
objectTest::CopyConstructor(void)
{
  Object object(m_object);

  CPPUNIT_ASSERT_EQUAL(m_object, object);
}

void
objectTest::GetSet(void)
{
  Object object;

  object.set_name("titi");
  CPPUNIT_ASSERT_EQUAL(std::string("titi"), object.get_name());

  object.set_description("nouveau titi");
  CPPUNIT_ASSERT_EQUAL(std::string("nouveau titi"), object.get_description());
}
