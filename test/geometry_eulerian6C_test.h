#ifndef _GEOMETRY_EULERIAN6C_TEST_H
#define _GEOMETRY_EULERIAN6C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "eulerian6C_geometry.h"

class GeometryEulerian6CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryEulerian6CTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( setAngles );
    CPPUNIT_TEST( get_sample_quaternion );
    CPPUNIT_TEST( get_sample_rotation_matrix );
    CPPUNIT_TEST( get_Q );
    CPPUNIT_TEST( get_kf );
    CPPUNIT_TEST( get_distance );
    CPPUNIT_TEST( setFromGeometry );

    CPPUNIT_TEST_SUITE_END();

    hkl::eulerian6C::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void otherConstructors(void);
    void setAngles(void);
    void addSampleDetectorAxe(void);
    void operateurs(void);
    void get_sample_quaternion(void);
    void get_sample_rotation_matrix(void);
    void get_Q(void);
    void get_kf(void);
    void get_distance(void);
    void setFromGeometry(void);
  };

#endif /* _GEOMETRY_EULERIAN6C_TEST_H */
