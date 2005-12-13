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

class axeTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( axeTest );
  CPPUNIT_TEST( testConstructeur1 );
  CPPUNIT_TEST( testEqual );
  CPPUNIT_TEST( testSet );
  CPPUNIT_TEST( testAsQuaternion );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void testConstructeur1(void);
  void testEqual(void);
  void testSet(void);
  void testAsQuaternion(void);
  void persistanceIO(void);
};

#endif //_AXE_TEST_H_
