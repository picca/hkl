// File to test matrix and vector implementation.

#ifndef SOURCE_TEST_H
#define SOURCE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "source.h"
#include "svecmat.h"
#include "constants.h"

using namespace hkl;

class sourceTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( sourceTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( SetWaveLength );
  CPPUNIT_TEST( SetDirection );
  CPPUNIT_TEST( GetSetKi );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  svector m_v;
  
  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void CopyConstructor(void);
  void SetWaveLength(void);
  void SetDirection(void);
  void GetSetKi(void);
  void persistanceIO(void);
};

#endif //SOURCE_TEST_H
