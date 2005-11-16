#ifndef _PSEUDOAXE_EULERIAN4C_TEST_H_
#define _PSEUDOAXE_EULERIAN4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "constants.h"
#include "pseudoaxe_eulerian4C.h"
#include "geometry_eulerian4C.h"

using std::vector;
using namespace hkl;

class PseudoAxe_Eulerian4C_Test : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( PseudoAxe_Eulerian4C_Test );
  
  CPPUNIT_TEST( Psi );
  
  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry_E4C;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Psi(void);
};

#endif //_PSEUDOAXE_EULERIAN4C_TEST_H_
