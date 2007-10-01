#ifndef _HKLOBJECT_TEST_H_
#define _HKLOBJECT_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "hklobject.h"

using namespace hkl;

class HKLObjectTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HKLObjectTest );
    CPPUNIT_TEST( constructors );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void constructors(void);
  };

#endif //_HKLOBJECT_TEST_H_
