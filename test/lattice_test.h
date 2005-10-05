// File to test matrix and vector implementation.

#ifndef LATTICE_TEST_H
#define LATTICE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include "lattice.h"
#include "constants.h"

class LatticeTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( LatticeTest );
  CPPUNIT_TEST( Constructor1 );
  CPPUNIT_TEST( Constructor2 );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetSet );
  CPPUNIT_TEST( ComputeReciprocalLattice );
  
  CPPUNIT_TEST_SUITE_END();

  double m_epsilon;
  double m_degToRad;
  double m_tau;

  public:
  
  void setUp();
  void tearDown();
  
  void Constructor1();
  void Constructor2();
  void Equal();
  void CopyConstructor();
  void GetSet();
  void ComputeReciprocalLattice();
};

#endif //LATTICE_TEST_H
