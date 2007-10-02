#ifndef SOURCE_TEST_H
#define SOURCE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "source.h"

class sourceTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( sourceTest );

    CPPUNIT_TEST( hkl_source_cmp );
    CPPUNIT_TEST( hkl_source_get_ki );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void hkl_source_cmp(void);
    void hkl_source_get_ki(void);
  };

#endif //SOURCE_TEST_H
