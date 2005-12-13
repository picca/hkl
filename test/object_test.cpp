#include "object_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( objectTest );

void
objectTest::setUp(void)
{
  m_object = Object("object", "nouvel object");
}

void 
objectTest::tearDown(void) 
{
}

void 
objectTest::Constructor(void)
{
  Object object("object");
  Object object2("object", "nouvel object");
  
  CPPUNIT_ASSERT_THROW(Object("", ""), HKLException);
  CPPUNIT_ASSERT_EQUAL(std::string("object"), object.get_name());
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

void
objectTest::persistanceIO(void)
{
  Object object_ref("ca le fait grave", "de la balle je vous le dit\ncoucou");
  Object object;
  Object object1_ref("another object", "with a nice description");
  Object object1("toto", "titi");
  
  stringstream flux;
  object_ref.toStream(flux);
  object1_ref.toStream(flux);
  object1.fromStream(flux);
  object.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(object_ref, object);
  CPPUNIT_ASSERT_EQUAL(object1_ref, object1);
}
