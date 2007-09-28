#ifndef _MODE_TWOC_TEST_H_
#define _MODE_TWOC_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <sstream>
#include <vector>

#include "source.h"
#include "sample_monocrystal.h"
#include "twoC_vertical_mode.h"

class Mode_TwoC_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Mode_TwoC_Test );

    CPPUNIT_TEST( Symetric );
    CPPUNIT_TEST( Fix_Incidence );

    CPPUNIT_TEST_SUITE_END();

    hkl::sample::MonoCrystal * _sample;
    hkl::twoC::vertical::Geometry _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Symetric(void);
    void Fix_Incidence(void);
  };

#endif //_MODE_TWOC_TEST_H_
