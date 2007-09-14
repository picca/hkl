#ifndef _QUATERNION_TEST_H_
#define _QUATERNION_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "quaternion.h"

class QuaternionTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( QuaternionTest );

    CPPUNIT_TEST( hkl_quaternion_cmp );
    CPPUNIT_TEST( hkl_quaternion_from_svector );
    CPPUNIT_TEST( hkl_quaternion_from_angle_and_axe );
    CPPUNIT_TEST( hkl_quaternion_times_quaternion );
    CPPUNIT_TEST( hkl_quaternion_norm2 );
    CPPUNIT_TEST( hkl_quaternion_conjugate );
    CPPUNIT_TEST( hkl_quaternion_to_smatrix );
    CPPUNIT_TEST( hkl_quaternion_to_angle_and_axe );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void hkl_quaternion_cmp(void);
    void hkl_quaternion_from_svector(void);
    void hkl_quaternion_from_angle_and_axe(void);
    void hkl_quaternion_times_quaternion(void);
    void hkl_quaternion_norm2(void);
    void hkl_quaternion_conjugate(void);
    void hkl_quaternion_to_smatrix(void);
    void hkl_quaternion_to_angle_and_axe(void);
  };

#endif //_QUATERNION_TEST_H_
