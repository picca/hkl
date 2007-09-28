#ifndef _GEOMETRY_EULERIAN4C_TEST_H
#define _GEOMETRY_EULERIAN4C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "eulerian4C_vertical_geometry.h"

class GeometryEulerian4CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryEulerian4CTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( get_sample_quaternion );
    CPPUNIT_TEST( get_sample_rotation_matrix );
    CPPUNIT_TEST( get_Q );
    CPPUNIT_TEST( get_distance );
    CPPUNIT_TEST( setFromGeometry );

    CPPUNIT_TEST_SUITE_END();

    hkl::eulerian4C::vertical::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void otherConstructors(void);
    void addSampleDetectorAxe(void);
    void operateurs(void);
    void get_sample_quaternion(void);
    void get_sample_rotation_matrix(void);
    void get_Q(void);
    void get_distance(void);
    void setFromGeometry(void);
  };

#endif /* _GEOMETRY_EULERIAN4C_TEST_H */
