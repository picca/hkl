#ifndef _MODE_KAPPA6C_TEST_H_
#define _MODE_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <sstream>

#include "sample_monocrystal.h"
#include "kappa6C_mode.h"

class Mode_Kappa6C_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Mode_Kappa6C_Test );

    CPPUNIT_TEST( Bissector );
    CPPUNIT_TEST( Delta_Theta );
    CPPUNIT_TEST( Constant_Omega );
    CPPUNIT_TEST( Constant_Chi );
    CPPUNIT_TEST( Constant_Phi );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::sample::MonoCrystal * _sample;
    hkl::kappa6C::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Bissector(void);
    void Delta_Theta(void);
    void Constant_Omega(void);
    void Constant_Chi(void);
    void Constant_Phi(void);
    void persistanceIO(void);
  };

#endif //_MODE_KAPPA6C_TEST_H_
