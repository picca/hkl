#ifndef _PSEUDOAXE_EULERIAN6C_TEST_H_
#define _PSEUDOAXE_EULERIAN6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "constants.h"
#include "pseudoaxe_eulerian6C.h"
#include "geometry_eulerian6C.h"

using namespace std;
using namespace hkl;

class PseudoAxe_Eulerian6C_Vertical_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_Eulerian6C_Vertical_Test );

    CPPUNIT_TEST( Tth );
    CPPUNIT_TEST( Q );
    CPPUNIT_TEST( Psi );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::geometry::Eulerian6C m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Tth(void);
    void Q(void);
    void Psi(void);
    void persistanceIO(void);
  };

#endif //_PSEUDOAXE_EULERIAN6C_TEST_H_
