#include "object_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( objectTest );

void
objectTest::setUp()
{
  m_object = Object("object");
}

void 
objectTest::tearDown() 
{
}

void 
objectTest::Constructor()
{
  Object object("object");
  
  CPPUNIT_ASSERT_EQUAL(std::string("object"), object.get_name());
}

void 
objectTest::Equal()
{
  Object object("object");
  
  CPPUNIT_ASSERT_EQUAL(m_object, object);
}

void
objectTest::CopyConstructor()
{
  Object object(m_object);
  
  CPPUNIT_ASSERT_EQUAL(m_object, object);
}

void
objectTest::GetSet()
{
  Object object("object");
  
  object.set_name("titi");
  CPPUNIT_ASSERT_EQUAL(std::string("titi"), object.get_name());
}
