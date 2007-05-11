#ifndef _DIFFRACTOMETER_TWOC_TEST_H_
#define _DIIFRACTOMETER_TWOC_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "twoC_vertical_diffractometer.h"

class Diffractometer_TwoC_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Diffractometer_TwoC_Test );

    //CPPUNIT_TEST( Geometry );
    //CPPUNIT_TEST( CrystalPart );
    //CPPUNIT_TEST( renameCrystal );
    //CPPUNIT_TEST( delCrystal );
    //CPPUNIT_TEST( delAllCrystals );
    //CPPUNIT_TEST( GetSetLattice );
    //CPPUNIT_TEST( GetReciprocalLattice );
    //CPPUNIT_TEST( getCrystalParametersNames );
    //CPPUNIT_TEST( AddReflection );
    //CPPUNIT_TEST( DelReflection );
    //CPPUNIT_TEST( GetReflection );
    //CPPUNIT_TEST( ModePart );
    //CPPUNIT_TEST( ComputeU );
    //CPPUNIT_TEST( ComputeHKL );
    //CPPUNIT_TEST( ComputeAngles );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::twoC::vertical::Diffractometer * _diffractometer;

  public:

    void setUp(void);
    void tearDown(void);

    /*
        void Geometry(void);
        void CrystalPart(void);
        void renameCrystal(void);
        void delCrystal(void);
        void delAllCrystals(void);
        void getCrystalParametersNames(void);
        void GetSetLattice(void);
        void GetReciprocalLattice(void);
        void AddReflection(void);
        void DelReflection(void);
        void GetReflection(void);
        void ModePart(void);
        void ComputeU(void);
        void ComputeHKL(void);
        void ComputeAngles(void);
    */
    void persistanceIO(void);
  };

#endif //_DIFFRACTOMETER_TWOC_TEST_H_
