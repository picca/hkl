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

class axeTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( axeTest );
  CPPUNIT_TEST( testConstructeur1 );
  CPPUNIT_TEST( testEqual );
  CPPUNIT_TEST( testSet );
  CPPUNIT_TEST( testAsQuaternion );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp();
  void tearDown();
  
  void testConstructeur1();
  void testEqual();
  void testSet();
  void testAsQuaternion();
};

#endif //_AXE_TEST_H_
