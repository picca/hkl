#ifndef _DIFFRACTOMETER_KAPPA6C_TEST_H_
#define _DIIFRACTOMETER_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "diffractometer_kappa6C.h"

using namespace hkl;

class DiffractometerKappa6CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( DiffractometerKappa6CTest );

    /*
        CPPUNIT_TEST( constructor );
        CPPUNIT_TEST( getSetAxes );
    */
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  public:
    void setUp(void);
    void tearDown(void);
    /*
        void constructor(void);
        void getSetAxes(void);
    */
    void persistanceIO(void);
  };

#endif //_DIFFRACTOMETER_KAPPA6C_TEST_H_
