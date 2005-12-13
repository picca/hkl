#ifndef _CRYSTALLIST_TEST_H_
#define _CRYSTALLIST_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "constants.h"
#include "crystallist.h"

using namespace std;
using namespace hkl;

class CrystalListTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( CrystalListTest );
  
  CPPUNIT_TEST( constructor );
  CPPUNIT_TEST( remove );
  CPPUNIT_TEST( clear );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp(void);
  void tearDown(void);
 
  void constructor(void);
  void remove(void);
  void clear(void);
};

#endif //_CRYSTALLIST_TEST_H_
