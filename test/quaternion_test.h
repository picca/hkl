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
  CPPUNIT_TEST( Constructor1 );
  CPPUNIT_TEST( Constructor2 );
  CPPUNIT_TEST( Constructor3 );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( Affectation );
  CPPUNIT_TEST( PlusEqual );
  CPPUNIT_TEST( MinusEqual );
  CPPUNIT_TEST( TimesEqual );
  CPPUNIT_TEST( DivideEqual );
  CPPUNIT_TEST( Norm2 );
  CPPUNIT_TEST( Conjugate );
  CPPUNIT_TEST( DotProduct );
  CPPUNIT_TEST( Invert );
  CPPUNIT_TEST( AsMatrix );
  CPPUNIT_TEST( getAngleAndAxe );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor1(void);
  void Constructor2(void);
  void Constructor3(void);
  void CopyConstructor(void);
  void Equal(void);
  void Affectation(void);
  void PlusEqual(void);
  void MinusEqual(void);
  void TimesEqual(void);
  void DivideEqual(void);
  void Norm2(void);
  void Conjugate(void);
  void DotProduct(void);
  void Invert(void);
  void AsMatrix(void);
  void getAngleAndAxe(void);
};

#endif //_QUATERNION_TEST_H_
