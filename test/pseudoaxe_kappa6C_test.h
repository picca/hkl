#ifndef _PSEUDOAXE_KAPPA6C_TEST_H_
#define _PSEUDOAXE_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "pseudoaxe_kappa6C.h"
#include "geometry_kappa6C.h"

using namespace std;
using namespace hkl;

class PseudoAxe_Kappa6C_Test : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( PseudoAxe_Kappa6C_Test );

  CPPUNIT_TEST( Omega );
  CPPUNIT_TEST( Chi );
  CPPUNIT_TEST( Phi );
  CPPUNIT_TEST( Psi );
  CPPUNIT_TEST( persistanceIO );

  CPPUNIT_TEST_SUITE_END();

  double m_alpha;
  hkl::geometry::Kappa6C * m_geometry_K6C;
  hkl::geometry::kappa4C::Vertical * m_geometry_K4C;
  hkl::geometry::eulerian4C::Vertical * m_geometry_E4C;

  public:

  void setUp(void);
  void tearDown(void);

  void Omega(void);
  void Chi(void);
  void Phi(void);
  void Psi(void);
  void persistanceIO(void);
};

#endif //_PSEUDOAXE_KAPPA6C_TEST_H_
