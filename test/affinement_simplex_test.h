#ifndef _AFFINEMENT_SIMPLEX_TEST_H_
#define _AFFINEMENT_SIMPLEX_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "crystal.h"
#include "constants.h"
#include "affinement.h"
#include "geometry_eulerian4C.h"

using std::vector;
using namespace hkl;

class Affinement_SimplexTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( Affinement_SimplexTest );
  
  CPPUNIT_TEST( Fit );
  CPPUNIT_TEST( Fit2 );
  
  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry_E4C;
  Crystal m_crystal;
  affinement::Simplex m_simplex;

  public:
  
  void setUp(void);
  void tearDown(void);
 
  void Fit(void);
  void Fit2(void);
};

#endif //_AFFINEMENT_SIMPLEX_TEST_H_
