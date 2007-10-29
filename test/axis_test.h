#ifndef _AXIS_TEST_H_
#define _AXIS_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include <axis.h>

class AxisTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( AxisTest );
    CPPUNIT_TEST( hkl_axes_add_rotation );
    CPPUNIT_TEST( hkl_axes_get_distance );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void hkl_axes_add_rotation(void);
    void hkl_axes_get_distance(void);
  };

#endif //_AXIS_TEST_H_
