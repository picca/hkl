#include "mystring_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( MyStringTest );

void
MyStringTest::setUp(void)
{}

void
MyStringTest::tearDown(void)
{}

void
MyStringTest::persistanceIO(void)
{
  MyString mystring_ref("de la balle je vous le dit\ncoucou");
  MyString mystring;
  MyString mystring1_ref("");
  MyString mystring1("toto");

  std::stringstream flux;
  mystring_ref.toStream(flux);
  mystring1_ref.toStream(flux);
  mystring.fromStream(flux);
  mystring1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(mystring_ref, mystring);
  CPPUNIT_ASSERT_EQUAL(mystring1_ref, mystring1);
}
