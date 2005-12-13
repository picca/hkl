#ifndef _RANGE_TEST_H
#define _RANGE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "range.h"

using namespace hkl;

class rangeTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( rangeTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetSet );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  Range m_range;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void CopyConstructor(void);
  void GetSet(void);
  void persistanceIO(void);
};

#endif //_RANGE_TEST_H
