// File to test matrix and vector implementation.

#ifndef _AXE_TEST_H_
#define _AXE_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>
#include "axe.h"
#include "constants.h"

using std::vector;
using namespace hkl;

class AxeTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( AxeTest );
  CPPUNIT_TEST( constructors );
  CPPUNIT_TEST( set );
  CPPUNIT_TEST( asQuaternion );
  CPPUNIT_TEST( getDistance );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

 public:
  
  void setUp(void);
  void tearDown(void);
  
  void constructors(void);
  void set(void);
  void asQuaternion(void);
  void getDistance(void);
  void persistanceIO(void);
};

#endif //_AXE_TEST_H_
