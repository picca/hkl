#ifndef SAMPLE_TEST_H
#define SAMPLE_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "sample_monocrystal.h"
#include "geometry_eulerian4C.h"

using namespace hkl;

class SampleTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( SampleTest );
    CPPUNIT_TEST( Constructor );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( clone );
    CPPUNIT_TEST( ComputeU );
    CPPUNIT_TEST( Fitness );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::Sample * _sample;
    hkl::geometry::eulerian4C::Vertical _geometry;
    hkl::Source _source;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructor(void);
    void Equal(void);
    void clone(void);
    void ComputeU(void);
    void Fitness(void);
    void persistanceIO(void);
  };

#endif //SAMPLE_TEST_H
