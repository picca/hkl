#ifndef _PSEUDOAXE_EULERIAN4C_TEST_H_
#define _PSEUDOAXE_EULERIAN4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "constants.h"
#include "pseudoaxe_eulerian4C.h"
#include "geometry_eulerian4C.h"

using namespace std;
using namespace hkl;

class PseudoAxe_Eulerian4C_Vertical_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_Eulerian4C_Vertical_Test );

    CPPUNIT_TEST( Psi );
    CPPUNIT_TEST( Th2th );
    CPPUNIT_TEST( Q2th );
    CPPUNIT_TEST( Q );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::geometry::eulerian4C::Vertical m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Psi(void);
    void Th2th(void);
    void Q2th(void);
    void Q(void);
    void persistanceIO(void);
  };

#endif //_PSEUDOAXE_EULERIAN4C_TEST_H_
