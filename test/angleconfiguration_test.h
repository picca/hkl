#ifndef _ANGLECONFIGURATION_TEST_H
#define _ANGLECONFIGURATION_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include "svecmat.h"
#include "constants.h"
#include "angleconfiguration_eulerian4C.h"
#include "angleconfiguration_eulerian6C.h"

using namespace hkl;

class angleConfigurationTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( angleConfigurationTest );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetAxesNames );
  CPPUNIT_TEST( AddSampleDetectorAxe );
  CPPUNIT_TEST( Operateurs );
  CPPUNIT_TEST( GetSampleRotationMatrix );
  CPPUNIT_TEST( GetQ );
  
  CPPUNIT_TEST_SUITE_END();

  angleConfiguration::Eulerian4C m_aC_E4C;
  angleConfiguration::Eulerian6C m_aC_E6C;
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

#endif /* _ANGLECONFIGURATION_TEST_H */