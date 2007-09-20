// File to test matrix and vector implementation.

#ifndef _AXE_ROTATION_TEST_H_
#define _AXE_ROTATION_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include <axe_rotation.h>

class AxeRotationTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( AxeRotationTest );
    CPPUNIT_TEST( constructors );
    CPPUNIT_TEST( set );
    CPPUNIT_TEST( get_distance );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void constructors(void);
    void set(void);
    void get_distance(void);
  };

#endif //_AXE_ROTATION_TEST_H_
