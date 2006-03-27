#ifndef _GEOMETRY_EULERIAN4C_TEST_H
#define _GEOMETRY_EULERIAN4C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "geometry_eulerian4C.h"

using namespace hkl;

class GeometryEulerian4CTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( GeometryEulerian4CTest );
  CPPUNIT_TEST( equal );
  CPPUNIT_TEST( copyConstructor );
  CPPUNIT_TEST( otherConstructors );
  CPPUNIT_TEST( getAxesNames );
  CPPUNIT_TEST( getSampleQuaternion );
  CPPUNIT_TEST( getSampleRotationMatrix );
  CPPUNIT_TEST( getQ );
  CPPUNIT_TEST( getDistance );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry;
  
  public:
 
  void setUp(void);
  void tearDown(void);
 
  void equal(void);
  void copyConstructor(void);
  void otherConstructors(void);
  void getAxesNames(void);
  void addSampleDetectorAxe(void);
  void operateurs(void);
  void getSampleQuaternion(void);
  void getSampleRotationMatrix(void);
  void getQ(void);
  void getDistance(void);
  void persistanceIO(void);
};

#endif /* _GEOMETRY_EULERIAN4C_TEST_H */
