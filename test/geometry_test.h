#ifndef _GEOMETRY_TEST_H
#define _GEOMETRY_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "geometry.h"

using namespace hkl;

class GeometryTest : public CppUnit::TestFixture  {
    CPPUNIT_TEST_SUITE( GeometryTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( isValid );
    CPPUNIT_TEST( addSampleDetectorAxe );
    CPPUNIT_TEST( operateurs );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    Geometry m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void isValid(void);
    void addSampleDetectorAxe(void);
    void operateurs(void);
    void persistanceIO(void);
};

#endif /* _GEOMETRY_TEST_H */
