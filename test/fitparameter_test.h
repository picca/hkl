#ifndef _FITPARAMETER_TEST_H_
#define _FITPARAMETER_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "fitparameter.h"

using namespace hkl;

class FitParameterTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( FitParameterTest );
    CPPUNIT_TEST( constructors );
    CPPUNIT_TEST( getSet );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void constructors(void);
    void getSet(void);
  };

#endif //_FITPARAMETER_TEST_H_
