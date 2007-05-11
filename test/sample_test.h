#ifndef SAMPLE_TEST_H
#define SAMPLE_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "sample_monocrystal.h"
#include "eulerian4C_vertical_geometry.h"

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
    hkl::eulerian4C::vertical::Geometry _geometry;
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
