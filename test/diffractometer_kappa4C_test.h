#ifndef _DIFFRACTOMETER_KAPPA4C_TEST_H_
#define _DIIFRACTOMETER_KAPPA4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "kappa4C_vertical_diffractometer.h"

class DiffractometerKappa4CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( DiffractometerKappa4CTest );
    CPPUNIT_TEST( pseudoAxes );

    CPPUNIT_TEST_SUITE_END();

  public:
    void setUp(void);
    void tearDown(void);
    void pseudoAxes(void);
  };

#endif //_DIFFRACTOMETER_KAPPA4C_TEST_H_
