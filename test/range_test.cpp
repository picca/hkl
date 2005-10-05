#include "range_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( rangeTest );

void
rangeTest::setUp()
{
  m_range = Range("range", 0., -1., 1.);
}

void 
rangeTest::tearDown() 
{
}

void 
rangeTest::Constructor()
{
  Range range("range", 0., -1., 1.);
  
  CPPUNIT_ASSERT_EQUAL(-1., range.get_min());
  CPPUNIT_ASSERT_EQUAL(1., range.get_max());
}

void 
rangeTest::Equal()
{
  Range range("range", 0., -1., 1.);
  
  CPPUNIT_ASSERT_EQUAL(m_range, range);
}

void
rangeTest::CopyConstructor()
{
  Range range(m_range);
  
  CPPUNIT_ASSERT_EQUAL(m_range, range);
}

void
rangeTest::GetSet()
{
  Range range("range", 0., -1, 1.);
  
  range.set_min(2.);
  range.set_max(5.);
  CPPUNIT_ASSERT_EQUAL(2., range.get_min());
  CPPUNIT_ASSERT_EQUAL(5., range.get_max());
}
