#ifndef _PSEUDOAXE_KAPPA4C_TEST_H_
#define _PSEUDOAXE_KAPPA4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "kappa4C_vertical_pseudoaxeengine.h"

class PseudoAxe_Kappa4C_Vertical_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_Kappa4C_Vertical_Test );

    CPPUNIT_TEST( Omega );
    CPPUNIT_TEST( Chi );
    CPPUNIT_TEST( Phi );
    CPPUNIT_TEST( Psi );
    CPPUNIT_TEST( Th2th );
    CPPUNIT_TEST( Q2th );
    CPPUNIT_TEST( Q );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    double m_alpha;
    hkl::kappa4C::vertical::Geometry * _geometry;
    hkl::eulerian4C::vertical::Geometry * _geometry_E4C;
    hkl::SampleList * _samples;

  public:

    void setUp(void);
    void tearDown(void);

    void Omega(void);
    void Chi(void);
    void Phi(void);
    void Psi(void);
    void Th2th(void);
    void Q2th(void);
    void Q(void);
    void persistanceIO(void);
  };

#endif //_PSEUDOAXE_KAPPA4C_TEST_H_
