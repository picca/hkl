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
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetAxesNames );
  CPPUNIT_TEST( AddSampleDetectorAxe );
  CPPUNIT_TEST( Operateurs );
  CPPUNIT_TEST( GetSampleRotationMatrix );
  CPPUNIT_TEST( GetQ );
  
  CPPUNIT_TEST_SUITE_END();

  geometry::Eulerian4C m_geometry_E4C;
  geometry::Eulerian6C m_geometry_E6C;
  svector m_v;
  
  public:
 
  void setUp();
  void tearDown();
 
  void Equal();
  void CopyConstructor();
  void GetAxesNames();
  void AddSampleDetectorAxe();
  void Operateurs();
  void GetSampleRotationMatrix();
  void GetQ();
};

#endif /* _GEOMETRY_TEST_H */
