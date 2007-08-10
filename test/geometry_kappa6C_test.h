#ifndef _GEOMETRY_KAPPA6C_TEST_H
#define _GEOMETRY_KAPPA6C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "kappa6C_geometry.h"

class GeometryKappa6CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryKappa6CTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( getSampleQuaternion );
    CPPUNIT_TEST( getSampleRotationMatrix );
    CPPUNIT_TEST( getQ );
    CPPUNIT_TEST( get_distance );
    CPPUNIT_TEST( setFromGeometry );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    double m_alpha;
    hkl::kappa6C::Geometry * m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void otherConstructors(void);
    void getSampleQuaternion(void);
    void getSampleRotationMatrix(void);
    void getQ(void);
    void get_distance(void);
    void setFromGeometry(void);
    void persistanceIO(void);
  };

#endif /* _GEOMETRY_KAPPA6C_TEST_H */
