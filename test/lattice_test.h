// File to test matrix and vector implementation.

#ifndef LATTICE_TEST_H
#define LATTICE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include "lattice.h"

using hkl::HKLException;

class LatticeTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( LatticeTest );
    CPPUNIT_TEST( constructors );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( reciprocal );
    CPPUNIT_TEST( get_B );
    CPPUNIT_TEST( randomize );

    CPPUNIT_TEST_SUITE_END();

    hkl::Lattice _lattice;

  public:

    void setUp(void);
    void tearDown(void);

    void constructors(void);
    void equal(void);
    void reciprocal(void);
    void get_B(void);
    void randomize(void);
  };

#endif //LATTICE_TEST_H
