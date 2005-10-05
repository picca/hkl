#ifndef _REFLECTION_TEST_H
#define _REFLECTION_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <string>

#include "source.h"
#include "constants.h"
#include "reflection.h"
#include "angleconfiguration_eulerian4C.h"

using std::cout;
using std::string;
using namespace hkl;

class reflectionTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( reflectionTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( GetSet );
  CPPUNIT_TEST( GetHKL );
  CPPUNIT_TEST( ComputeAngle );
  CPPUNIT_TEST( GetSampleRotationMatrix );
  CPPUNIT_TEST( GetQ );

  CPPUNIT_TEST_SUITE_END();

  angleConfiguration::Eulerian4C m_aC_E4C;
  Source m_source;
  
  public:
  
  void setUp();
  void tearDown();
  
  void Constructor();
  void Equal();
  void GetSet();
  void GetHKL();
  void ComputeAngle();
  void GetSampleRotationMatrix();
  void GetQ();
};

#endif /* _REFLECTION_TEST_H */
