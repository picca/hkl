#ifndef _GEOMETRY_KAPPA4C_TEST_H
#define _GEOMETRY_KAPPA4C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "geometry_kappa4C.h"

using namespace hkl;

class GeometryKappa4CTest : public CppUnit::TestFixture  {
    CPPUNIT_TEST_SUITE( GeometryKappa4CTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( get_alpha );
    CPPUNIT_TEST( getAxesNames );
    CPPUNIT_TEST( getSampleQuaternion );
    CPPUNIT_TEST( getSampleRotationMatrix );
    CPPUNIT_TEST( getQ );
    CPPUNIT_TEST( getDistance );
    CPPUNIT_TEST( setAngles );
    CPPUNIT_TEST( setFromGeometry );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    double m_alpha;
    geometry::kappa4C::Vertical * m_geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void otherConstructors(void);
    void get_alpha(void);
    void getAxesNames(void);
    void getSampleQuaternion(void);
    void getSampleRotationMatrix(void);
    void getQ(void);
    void getDistance(void);
    void setAngles(void);
    void setFromGeometry(void);
    void persistanceIO(void);
};

#endif /* _GEOMETRY_KAPPA4C_TEST_H */
