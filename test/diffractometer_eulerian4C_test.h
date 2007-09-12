#ifndef _DIFFRACTOMETER_EULERIAN4C_TEST_H_
#define _DIIFRACTOMETER_EULERIAN4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "eulerian4C_vertical_diffractometer.h"

class DiffractometerEulerian4CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( DiffractometerEulerian4CTest );

    /*
        CPPUNIT_TEST( GetSetAxe );
        CPPUNIT_TEST( CrystalPart );
        CPPUNIT_TEST( renameCrystal );
        CPPUNIT_TEST( delCrystal );
        CPPUNIT_TEST( delAllCrystals );
        CPPUNIT_TEST( GetSetLattice );
        CPPUNIT_TEST( GetReciprocalLattice );
        CPPUNIT_TEST( getCrystalParametersNames );
        CPPUNIT_TEST( AddReflection );
        CPPUNIT_TEST( DelReflection );
        //  CPPUNIT_TEST( GetReflection );
        CPPUNIT_TEST( ModePart );
        //  CPPUNIT_TEST( ComputeU );
        CPPUNIT_TEST( ComputeHKL );
        CPPUNIT_TEST( ComputeAngles );
        CPPUNIT_TEST( LPS );
        CPPUNIT_TEST( LPS2 );
    */
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    /*
        void GetSetAxe(void);
        void CrystalPart(void);
        void renameCrystal(void);
        void delCrystal(void);
        void delAllCrystals(void);
        void getCrystalParametersNames(void);
        void GetSetLattice(void);
        void GetReciprocalLattice(void);
        void AddReflection(void);
        void DelReflection(void);
        //  void GetReflection(void);
        void ModePart(void);
        //  void ComputeU(void);
        void ComputeHKL(void);
        void ComputeAngles(void);
        void LPS(void);
        void LPS2(void);
    */
    void persistanceIO(void);
  };

#endif //_DIFFRACTOMETER_EULERIAN4C_TEST_H_
