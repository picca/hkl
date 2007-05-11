#ifndef _PSEUDOAXE_TWOC_TEST_H_
#define _PSEUDOAXE_TWOC_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "twoC_vertical_pseudoaxeengine.h"

class PseudoAxe_TwoC_Vertical_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_TwoC_Vertical_Test );

    CPPUNIT_TEST( Th2th );
    CPPUNIT_TEST( Q2th );
    CPPUNIT_TEST( Q );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::twoC::vertical::Geometry m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Th2th(void);
    void Q2th(void);
    void Q(void);
    void persistanceIO(void);
  };

#endif //_PSEUDOAXE_TWOC_TEST_H_
