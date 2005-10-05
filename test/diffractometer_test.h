#ifndef _DIFFRACTOMETER_TEST_H_
#define _DIIFRACTOMETER_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>
#include "diffractometer.h"
#include "constants.h"

using std::vector;

class diffractometerTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( diffractometerTest );
  
  CPPUNIT_TEST( GetSetAxe );
//  CPPUNIT_TEST( CrystalPart );
  CPPUNIT_TEST( GetSetLattice );
//  CPPUNIT_TEST( GetReciprocalLattice );
  CPPUNIT_TEST( AddReflection );
//  CPPUNIT_TEST( DelReflection );
//  CPPUNIT_TEST( GetReflection );
  CPPUNIT_TEST( ModePart );
//  CPPUNIT_TEST( ComputeU );
//  CPPUNIT_TEST( ComputeHKL );
//  CPPUNIT_TEST( ComputeAngles );
//  CPPUNIT_TEST( LPS );
  
  CPPUNIT_TEST_SUITE_END();

  double m_epsilon;
  double m_degToRad;
  Source m_source;
  AngleConfiguration m_aC;

  public:
  
  void setUp();
  void tearDown();
 
  void GetSetAxe();
//  void CrystalPart();
  void GetSetLattice();
//  void GetReciprocalLattice();
  void AddReflection();
//  void DelReflection();
//  void GetReflection();
  void ModePart();
//  void ComputeU();
//  void ComputeHKL();
//  void ComputeAngles();
//  void LPS();
};

#endif //_DIFFRACTOMETER_TEST_H_
