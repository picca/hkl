#ifndef _INTERVAL_TEST_H
#define _INTERVAL_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <interval.h>

class IntervalTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( IntervalTest );
    CPPUNIT_TEST( Constructors );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( cos );
    CPPUNIT_TEST( acos );
    CPPUNIT_TEST( sin );
    CPPUNIT_TEST( asin );
    CPPUNIT_TEST( tan );
    CPPUNIT_TEST( atan );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::Interval _interval;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructors(void);
    void Equal(void);
    void GetSet(void);
    void operators(void);
    void cos(void);
    void acos(void);
    void sin(void);
    void asin(void);
    void tan(void);
    void atan(void);
    void persistanceIO(void);
  };

#endif //_RANGE_TEST_H
