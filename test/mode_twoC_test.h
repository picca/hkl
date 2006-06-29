#ifndef _MODE_TWOC_TEST_H_
#define _MODE_TWOC_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <sstream>
#include <vector>

#include "source.h"
#include "crystal.h"
#include "constants.h"
#include "mode_twoC.h"
#include "geometry_twoC.h"

using std::vector;
using namespace hkl;

class Mode_TwoC_Test : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( Mode_TwoC_Test );
  
  CPPUNIT_TEST( Symetric );
  CPPUNIT_TEST( Fix_Incidence );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  Crystal<geometry::twoC::Vertical> m_crystal;
  Source m_source;
  geometry::twoC::Vertical m_geometry;

  public:

  void setUp(void);
  void tearDown(void);

  void Symetric(void);
  void Fix_Incidence(void);
  void persistanceIO(void);
};

#endif //_MODE_TWOC_TEST_H_
