// File to test matrix and vector implementation.

#ifndef CRYSTAL_TEST_H
#define CRYSTAL_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "source.h"
#include "crystal.h"
#include "constants.h"
#include "geometry_eulerian4C.h"

using namespace hkl;

class CrystalTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( CrystalTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( PlusEqual );
  CPPUNIT_TEST( MinusEqual );
  CPPUNIT_TEST( TimesEqual );
  CPPUNIT_TEST( DivideEqual );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetLattice );
  CPPUNIT_TEST( SetLattice );
  CPPUNIT_TEST( GetReciprocalLattice );
  CPPUNIT_TEST( ReflectionPart );
  CPPUNIT_TEST( ComputeB );
  CPPUNIT_TEST( isEnoughReflections );
  CPPUNIT_TEST( ComputeU );
  CPPUNIT_TEST( Fitness );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  Crystal m_crystal;
  geometry::Eulerian4C m_geometry_E4C;
  Source m_source;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void PlusEqual(void);
  void MinusEqual(void);
  void TimesEqual(void);
  void DivideEqual(void);
  void CopyConstructor(void);
  void GetLattice(void);
  void SetLattice(void);
  void GetReciprocalLattice(void);
  void ReflectionPart(void);
  void ComputeB(void);
  void isEnoughReflections(void);
  void ComputeU(void);
  void Fitness(void);
  void persistanceIO(void);
};

#endif //CRYSTAL_TEST_H
