#ifndef _INTERVAL_TEST_H
#define _INTERVAL_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <interval.h>

class IntervalTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( IntervalTest );
    CPPUNIT_TEST( hkl_interval_cmp );
    CPPUNIT_TEST( hkl_interval_plus_interval );
    CPPUNIT_TEST( hkl_interval_plus_double );
    CPPUNIT_TEST( hkl_interval_minus_interval );
    CPPUNIT_TEST( hkl_interval_minus_double );
    CPPUNIT_TEST( hkl_interval_times_interval );
    CPPUNIT_TEST( hkl_interval_times_double );
    CPPUNIT_TEST( hkl_interval_divides_double );
    CPPUNIT_TEST( hkl_interval_contain_zero );
    CPPUNIT_TEST( hkl_interval_cos );
    CPPUNIT_TEST( hkl_interval_acos );
    CPPUNIT_TEST( hkl_interval_sin );
    CPPUNIT_TEST( hkl_interval_asin );
    CPPUNIT_TEST( hkl_interval_tan );
    CPPUNIT_TEST( hkl_interval_atan );
    CPPUNIT_TEST_SUITE_END();

    hkl_interval _interval;

  public:

    void setUp(void);
    void tearDown(void);

    void hkl_interval_cmp(void);
    void hkl_interval_plus_interval(void);
    void hkl_interval_plus_double(void);
    void hkl_interval_minus_interval(void);
    void hkl_interval_minus_double(void);
    void hkl_interval_times_interval(void);
    void hkl_interval_times_double(void);
    void hkl_interval_divides_double(void);
    void hkl_interval_contain_zero(void);
    void hkl_interval_cos(void);
    void hkl_interval_acos(void);
    void hkl_interval_sin(void);
    void hkl_interval_asin(void);
    void hkl_interval_tan(void);
    void hkl_interval_atan(void);
  };

#endif //_RANGE_TEST_H
