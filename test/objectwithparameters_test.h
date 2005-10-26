#ifndef _OBJECTWITHPARAMETERS_TEST_H_
#define _OBJECTWITHPARAMETERS_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "objectwithparameters.h"

using namespace hkl;

class ObjectWithParametersTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( ObjectWithParametersTest );
  CPPUNIT_TEST( equal );
  CPPUNIT_TEST( addParameter );
  
  CPPUNIT_TEST_SUITE_END();

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void equal(void);
  void addParameter(void);
};

#endif //_OBJECTWITHPARAMETERS_TEST_H_
