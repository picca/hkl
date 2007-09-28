#ifndef _AFFINEMENT_SIMPLEX_TEST_H_
#define _AFFINEMENT_SIMPLEX_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "affinement_simplex.h"
#include "samplefactory.h"
#include "eulerian4C_vertical_geometry.h"

using std::vector;

class Affinement_SimplexTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Affinement_SimplexTest );

    CPPUNIT_TEST( Fit );
    CPPUNIT_TEST( Fit2 );

    CPPUNIT_TEST_SUITE_END();

    hkl::affinement::Simplex _simplex;
    hkl::Sample * _sample;
    hkl::eulerian4C::vertical::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Fit(void);
    void Fit2(void);
  };

#endif //_AFFINEMENT_SIMPLEX_TEST_H_
