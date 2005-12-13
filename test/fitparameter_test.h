#ifndef _FITPARAMETER_TEST_H_
#define _FITPARAMETER_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "fitparameter.h"

using namespace hkl;

class fitParameterTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( fitParameterTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetSet );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  FitParameter m_fitParameter;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void CopyConstructor(void);
  void GetSet(void);
  void persistanceIO(void);
};

#endif //_FITPARAMETER_TEST_H_
