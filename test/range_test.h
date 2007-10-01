#ifndef _RANGE_TEST_H
#define _RANGE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "range.h"

using namespace hkl;

class rangeTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( rangeTest );
    CPPUNIT_TEST( Constructors );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( GetSet );

    CPPUNIT_TEST_SUITE_END();

    Range _range;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructors(void);
    void Equal(void);
    void GetSet(void);
  };

#endif //_RANGE_TEST_H
