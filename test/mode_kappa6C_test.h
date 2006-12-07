#ifndef _MODE_KAPPA6C_TEST_H_
#define _MODE_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <sstream>
#include <vector>

#include "crystal.h"
#include "constants.h"
#include "mode_kappa6C.h"
#include "geometry_kappa6C.h"

using std::vector;
using namespace hkl;

class Mode_Kappa6C_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Mode_Kappa6C_Test );

    CPPUNIT_TEST( Bissector );
    CPPUNIT_TEST( Delta_Theta );
    CPPUNIT_TEST( Constant_Omega );
    CPPUNIT_TEST( Constant_Chi );
    CPPUNIT_TEST( Constant_Phi );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    Crystal<geometry::Kappa6C> m_crystal;
    geometry::Kappa6C m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Bissector(void);
    void Delta_Theta(void);
    void Constant_Omega(void);
    void Constant_Chi(void);
    void Constant_Phi(void);
    void persistanceIO(void);
  };

#endif //_MODE_KAPPA6C_TEST_H_
