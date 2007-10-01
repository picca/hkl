#ifndef _PARAMETER_TEST_H_
#define _PARAMETER_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "parameter.h"

using namespace hkl;

class ParameterTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( ParameterTest );
    CPPUNIT_TEST( constructors );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void constructors(void);
  };

#endif //_PARAMETER_TEST_H_
