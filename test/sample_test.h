#ifndef SAMPLE_TEST_H
#define SAMPLE_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "sample_monocrystal.h"
#include "geometry_eulerian4C.h"

using namespace hkl;

class SampleTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( SampleTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  //CPPUNIT_TEST( PlusEqual );
  //CPPUNIT_TEST( MinusEqual );
  //CPPUNIT_TEST( TimesEqual );
  //CPPUNIT_TEST( DivideEqual );
  CPPUNIT_TEST( clone );
  //CPPUNIT_TEST( GetLattice );
  //CPPUNIT_TEST( SetLattice );
  //CPPUNIT_TEST( GetReciprocalLattice );
  //CPPUNIT_TEST( ReflectionPart );
  //CPPUNIT_TEST( ComputeB );
  //CPPUNIT_TEST( isEnoughReflections );
  CPPUNIT_TEST( ComputeU );
  CPPUNIT_TEST( Fitness );
  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  hkl::Sample * _sample;
  hkl::geometry::eulerian4C::Vertical _geometry;
  hkl::Source _source;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  //void PlusEqual(void);
  //void MinusEqual(void);
  //void TimesEqual(void);
  //void DivideEqual(void);
  void clone(void);
  //void GetLattice(void);
  //void SetLattice(void);
  //void GetReciprocalLattice(void);
  //void ReflectionPart(void);
  //void ComputeB(void);
  //void isEnoughReflections(void);
  void ComputeU(void);
  void Fitness(void);
  void persistanceIO(void);
};

#endif //SAMPLE_TEST_H
