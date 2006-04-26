#ifndef _MODE_KAPPA4C_TEST_H_
#define _MODE_KAPPA4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <sstream>
#include <vector>

#include "crystal.h"
#include "constants.h"
#include "mode_kappa4C.h"
#include "geometry_kappa4C.h"

using std::vector;
using namespace hkl;

class Mode_Kappa4C_Test : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( Mode_Kappa4C_Test );
  
  CPPUNIT_TEST( Bissector );
  CPPUNIT_TEST( Delta_Theta );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  Crystal m_crystal;
  geometry::kappa4C::Vertical * m_geometry;

  public:

  void setUp(void);
  void tearDown(void);

  void Bissector(void);
  void Delta_Theta(void);
  void persistanceIO(void);
};

#endif //_MODE_KAPPA4C_TEST_H_
