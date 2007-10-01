#ifndef _GEOMETRY_TEST_H
#define _GEOMETRY_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "geometry.h"

class GeometryTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( operateurs );

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::Geometry * m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void operateurs(void);
  };

#endif /* _GEOMETRY_TEST_H */
