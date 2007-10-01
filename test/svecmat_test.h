// File to test matrix and vector implementation.

#ifndef SVECMAT_TEST_H
#define SVECMAT_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "svector.h"

class vectorMatrixTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( vectorMatrixTest );

    CPPUNIT_TEST( hkl_svector_cmp );
    CPPUNIT_TEST( hkl_svector_is_opposite );
    CPPUNIT_TEST( hkl_svector_norm2 );
    CPPUNIT_TEST( hkl_svector_normalize );
    CPPUNIT_TEST( hkl_svector_scalar_product );
    CPPUNIT_TEST( hkl_svector_vectorial_product );
    CPPUNIT_TEST( hkl_svector_angle );
    CPPUNIT_TEST( hkl_svector_rotated_around_vector );
    CPPUNIT_TEST( hkl_svector_times_smatrix );

    CPPUNIT_TEST( hkl_smatrix_cmp );
    CPPUNIT_TEST( hkl_smatrix_from_euler );
    CPPUNIT_TEST( hkl_smatrix_from_two_svector );
    CPPUNIT_TEST( hkl_smatrix_times_svector );
    CPPUNIT_TEST( hkl_smatrix_times_smatrix );
    CPPUNIT_TEST( hkl_smatrix_transpose );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void hkl_svector_cmp(void);
    void hkl_svector_is_opposite(void);
    void hkl_svector_norm2(void);
    void hkl_svector_normalize(void);
    void hkl_svector_scalar_product(void);
    void hkl_svector_vectorial_product(void);
    void hkl_svector_angle(void);
    void hkl_svector_rotated_around_vector(void);
    void hkl_svector_minus_svector(void);
    void hkl_svector_times_smatrix(void);

    void hkl_smatrix_cmp(void);
    void hkl_smatrix_from_euler(void);
    void hkl_smatrix_from_two_svector(void);
    void hkl_smatrix_times_svector(void);
    void hkl_smatrix_times_smatrix(void);
    void hkl_smatrix_transpose(void);
  };

#endif //SVECMAT_TEST_H
