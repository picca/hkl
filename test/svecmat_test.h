// File to test matrix and vector implementation.

#ifndef SVECMAT_TEST_H
#define SVECMAT_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "svector.h"
#include "constant.h"

using namespace hkl;

class vectorMatrixTest : public CppUnit::TestFixture
  {
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
    CPPUNIT_TEST( Normalize );
    CPPUNIT_TEST( Scalar );
    CPPUNIT_TEST( VectorialProduct );
    CPPUNIT_TEST( Angle );
    CPPUNIT_TEST( AxisSystem );
    CPPUNIT_TEST( rotatedAroundVector );


    CPPUNIT_TEST( svector_TimesEqual_smatrix );
    CPPUNIT_TEST( smatrix_Times_svector );
    CPPUNIT_TEST( smatrix_TimesEqual_smatrix );
    CPPUNIT_TEST( smatrix_Times_smatrix );
    CPPUNIT_TEST( AsEulerian );

    CPPUNIT_TEST( svector_IO );
    CPPUNIT_TEST( smatrix_IO );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void SVectorConstructor1(void);
    void SVectorConstructor2(void);
    void SVectorEqual(void);
    void SVectorCopyConstructor(void);
    void SVectorSet(void);

    void SMatrixConstructor1(void);
    void SMatrixConstructor2(void);
    void SMatrixConstructor3(void);
    void SMatrixEqual(void);
    void SMatrixCopyConstructor(void);

    void Norm2(void);
    void NormInf(void);
    void Normalize(void);
    void Scalar(void);
    void VectorialProduct(void);
    void Angle(void);
    void AxisSystem(void);
    void rotatedAroundVector(void);

    void svector_MinusEqual_svector(void);
    void svector_Minus_svector(void);
    void svector_TimesEqual_smatrix(void);
    void svector_Times_smatrix(void);
    void smatrix_Times_svector(void);
    void smatrix_TimesEqual_smatrix(void);
    void smatrix_Times_smatrix(void);
    void AsEulerian(void);

    void svector_IO(void);
    void smatrix_IO(void);
  };

#endif //SVECMAT_TEST_H
