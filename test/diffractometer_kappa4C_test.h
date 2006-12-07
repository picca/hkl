#ifndef _DIFFRACTOMETER_KAPPA4C_TEST_H_
#define _DIIFRACTOMETER_KAPPA4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "diffractometer_kappa4C.h"

using namespace hkl;

class DiffractometerKappa4CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( DiffractometerKappa4CTest );

    CPPUNIT_TEST( constructor );
    CPPUNIT_TEST( getSetAxes );

    CPPUNIT_TEST_SUITE_END();

  public:
    diffractometer::kappa4C::Vertical m_diffractometer;

    void setUp(void);
    void tearDown(void);

    void constructor(void);
    void getSetAxes(void);
  };

#endif //_DIFFRACTOMETER_KAPPA4C_TEST_H_
