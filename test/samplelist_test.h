#ifndef _SAMPLELIST_TEST_H
#define _SAMPLELIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "samplelist.h"
#include "geometry_eulerian4C.h"

class SampleListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( SampleListTest );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::SampleList * _sampleList;
    hkl::geometry::eulerian4C::Vertical _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void operators(void);
    void persistanceIO(void);
  };

#endif /* _SAMPLELIST_TEST_H */
