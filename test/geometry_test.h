#ifndef _GEOMETRY_TEST_H
#define _GEOMETRY_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include "svecmat.h"
#include "constants.h"
#include "geometry.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"

using namespace hkl;

class GeometryTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( GeometryTest );
  CPPUNIT_TEST( equal );
  CPPUNIT_TEST( copyConstructor );
  CPPUNIT_TEST( getAxesNames );
  CPPUNIT_TEST( addSampleDetectorAxe );
  CPPUNIT_TEST( operateurs );
  CPPUNIT_TEST( getSampleQuaternion );
  CPPUNIT_TEST( getSampleRotationMatrix );
  CPPUNIT_TEST( getQ );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry_E4C;
  geometry::Eulerian6C m_geometry_E6C;
  svector m_v;
  
  public:
 
  void setUp(void);
  void tearDown(void);
 
  void equal(void);
  void copyConstructor(void);
  void getAxesNames(void);
  void addSampleDetectorAxe(void);
  void operateurs(void);
  void getSampleQuaternion(void);
  void getSampleRotationMatrix(void);
  void getQ(void);
  void persistanceIO(void);
};

#endif /* _GEOMETRY_TEST_H */
