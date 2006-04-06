#ifndef _PSEUDOAXE_KAPPA4C_TEST_H_
#define _PSEUDOAXE_KAPPA4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "pseudoaxe_kappa4C.h"
#include "geometry_kappa4C.h"

using namespace std;
using namespace hkl;

class PseudoAxe_Kappa4C_Test : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( PseudoAxe_Kappa4C_Test );

  CPPUNIT_TEST( Omega );
  CPPUNIT_TEST( Chi );
  CPPUNIT_TEST( Phi );
  CPPUNIT_TEST( persistanceIO );

  CPPUNIT_TEST_SUITE_END();

  double m_alpha;
  hkl::geometry::Kappa4C * m_geometry_K4C;

  public:

  void setUp(void);
  void tearDown(void);

  void Omega(void);
  void Chi(void);
  void Phi(void);
  void persistanceIO(void);
};

#endif //_PSEUDOAXE_KAPPA4C_TEST_H_
