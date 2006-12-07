#ifndef _MYSTRING_TEST_H_
#define _MYSTRING_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "mystring.h"

using namespace hkl;

class MyStringTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( MyStringTest );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void persistanceIO(void);
  };

#endif //_MYSTRING_TEST_H_
