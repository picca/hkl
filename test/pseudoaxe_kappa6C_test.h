#ifndef _PSEUDOAXE_KAPPA6C_TEST_H_
#define _PSEUDOAXE_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "kappa6C_pseudoaxeengine.h"

class PseudoAxe_Kappa6C_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_Kappa6C_Test );

    CPPUNIT_TEST( Omega );
    CPPUNIT_TEST( Chi );
    CPPUNIT_TEST( Phi );
    CPPUNIT_TEST( Psi );
    CPPUNIT_TEST( Tth );
    CPPUNIT_TEST( Q );

    CPPUNIT_TEST_SUITE_END();

    double _alpha;
    hkl::kappa6C::Geometry * _geometry;
    hkl::eulerian6C::Geometry * _geometry_E6C;
    hkl::kappa4C::vertical::Geometry * _geometry_K4C;
    hkl::eulerian4C::vertical::Geometry * _geometry_E4C;
    hkl::SampleList * _samples;

  public:

    void setUp(void);
    void tearDown(void);

    void Omega(void);
    void Chi(void);
    void Phi(void);
    void Psi(void);
    void Tth(void);
    void Q(void);
  };

#endif //_PSEUDOAXE_KAPPA6C_TEST_H_
