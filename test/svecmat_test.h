// File to test matrix and vector implementation.

#ifndef SVECMAT_TEST_H
#define SVECMAT_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "svecmat.h"
#include "constants.h"

using namespace hkl;

class vectorMatrixTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( vectorMatrixTest );
  CPPUNIT_TEST( SVectorConstructor1 );
  CPPUNIT_TEST( SVectorConstructor2 );
  CPPUNIT_TEST( SVectorEqual );
  CPPUNIT_TEST( SVectorCopyConstructor );
  CPPUNIT_TEST( SVectorSet );

  CPPUNIT_TEST( SMatrixConstructor1 );
  CPPUNIT_TEST( SMatrixConstructor2 );
  CPPUNIT_TEST( SMatrixConstructor3 );
  CPPUNIT_TEST( SMatrixEqual );
  CPPUNIT_TEST( SMatrixCopyConstructor );
  
  CPPUNIT_TEST( Norm2 );
  CPPUNIT_TEST( NormInf );
  CPPUNIT_TEST( Normalize );
  CPPUNIT_TEST( Scalar );
  CPPUNIT_TEST( VectorialProduct );
  CPPUNIT_TEST( Angle );
  CPPUNIT_TEST( AxisSystem );
  CPPUNIT_TEST( svector_TimesEqual_smatrix );
  CPPUNIT_TEST( smatrix_Times_svector );
  CPPUNIT_TEST( smatrix_TimesEqual_smatrix );
  CPPUNIT_TEST( smatrix_Times_smatrix );
  CPPUNIT_TEST( AsEulerian );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp();
  void tearDown();
  
  void SVectorConstructor1();
  void SVectorConstructor2();
  void SVectorEqual();
  void SVectorCopyConstructor();
  void SVectorSet();

  void SMatrixConstructor1();
  void SMatrixConstructor2();
  void SMatrixConstructor3();
  void SMatrixEqual();
  void SMatrixCopyConstructor();
  
  void Norm2();
  void NormInf();
  void Normalize();
  void Scalar();
  void VectorialProduct();
  void Angle();
  void AxisSystem();
  void svector_MinusEqual_svector();
  void svector_Minus_svector();
  void svector_TimesEqual_smatrix();
  void svector_Times_smatrix();
  void smatrix_Times_svector();
  void smatrix_TimesEqual_smatrix();
  void smatrix_Times_smatrix();
  void AsEulerian();
};

#endif //SVECMAT_TEST_H
