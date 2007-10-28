#ifndef _AXIS_TEST_H_
#define _AXIS_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include <axis.h>

class AxisTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( AxisTest );
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
