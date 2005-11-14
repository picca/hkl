#ifndef _REFLECTION_TEST_H
#define _REFLECTION_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <string>

#include "source.h"
#include "constants.h"
#include "reflection.h"
#include "geometry_eulerian4C.h"

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
  CPPUNIT_TEST( isColinear );

  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry_E4C;
  Source m_source;
  
  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void GetSet(void);
  void GetHKL(void);
  void ComputeAngle(void);
  void isColinear(void);
};

#endif /* _REFLECTION_TEST_H */
