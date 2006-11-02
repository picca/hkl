#ifndef _AFFINEMENT_SIMPLEX_TEST_H_
#define _AFFINEMENT_SIMPLEX_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "affinement.h"
#include "samplefactory.h"
#include "geometry_eulerian4C.h"

using std::vector;
using namespace hkl;

class Affinement_SimplexTest : public CppUnit::TestFixture
{
    CPPUNIT_TEST_SUITE( Affinement_SimplexTest );

    CPPUNIT_TEST( Fit );
    CPPUNIT_TEST( Fit2 );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    affinement::Simplex _simplex;
    Sample * _sample;
    geometry::eulerian4C::Vertical _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Fit(void);
    void Fit2(void);
    void persistanceIO(void);
};

#endif //_AFFINEMENT_SIMPLEX_TEST_H_
