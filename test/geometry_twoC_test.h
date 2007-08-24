#ifndef _GEOMETRY_TWOC_TEST_H
#define _GEOMETRY_TWOC_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "twoC_vertical_geometry.h"
#include "sample.h"

class GeometryTwoCTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryTwoCTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( get_sample_quaternion );
    CPPUNIT_TEST( get_sample_rotation_matrix );
    CPPUNIT_TEST( get_Q );
    CPPUNIT_TEST( get_distance );
    CPPUNIT_TEST( computeHKL );
    CPPUNIT_TEST( setFromGeometry );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::twoC::vertical::Geometry * _geometry;
    hkl::Lattice _lattice;

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
    void computeHKL(void);
    void setFromGeometry(void);
    void persistanceIO(void);
  };

#endif /* _GEOMETRY_TWOC_TEST_H */
