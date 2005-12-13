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

void
rangeTest::persistanceIO(void)
{
  Range range_ref("", 1.345748912435689e-4, -1.45e3, 1.34e12);
  
  Range range1_ref("son nom", 2.5e-4, 0, 0);
  range1_ref.set_description("On en met une longue\navec un saut de ligne.");  
  
  Range range;
  Range range1;
  
  stringstream flux;
  range_ref.toStream(flux);
  range1_ref.toStream(flux);
  range.fromStream(flux);
  range1.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(range_ref, range);
  CPPUNIT_ASSERT_EQUAL(range1_ref, range1);
}
