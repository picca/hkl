// File to test matrix and vector implementation.

#ifndef _QUATERNION_TEST_H_
#define _QUATERNION_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "quaternion.h"
#include "constants.h"

using std::vector;
using namespace hkl;

class quaternionTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( quaternionTest );
  CPPUNIT_TEST( testConstructor1 );
  CPPUNIT_TEST( testConstructor2 );
  CPPUNIT_TEST( testConstructor3 );
  CPPUNIT_TEST( testCopyConstructor );
  CPPUNIT_TEST( testEqual );
  CPPUNIT_TEST( testAffectation );
  CPPUNIT_TEST( testPlusEqual );
  CPPUNIT_TEST( testMinusEqual );
  CPPUNIT_TEST( testTimesEqual );
  CPPUNIT_TEST( testDivideEqual );
  CPPUNIT_TEST( testNorm2 );
  CPPUNIT_TEST( testConjugate );
  CPPUNIT_TEST( testDotProduct );
  CPPUNIT_TEST( testInvert );
  CPPUNIT_TEST( testAsMatrix );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp();
  void tearDown();
  
  void testConstructor1();
  void testConstructor2();
  void testConstructor3();
  void testCopyConstructor();
  void testEqual();
  void testAffectation();
  void testPlusEqual();
  void testMinusEqual();
  void testTimesEqual();
  void testDivideEqual();
  void testNorm2();
  void testConjugate();
  void testDotProduct();
  void testInvert();
  void testAsMatrix();
};

#endif //_QUATERNION_TEST_H_
