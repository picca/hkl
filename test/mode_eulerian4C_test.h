#ifndef _MODE_EULERIAN4C_TEST_H_
#define _MODE_EULERIAN4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "cristal.h"
#include "source.h"
#include "constants.h"
#include "mode_eulerian4C.h"
#include "angleconfiguration_eulerian4C.h"

using std::vector;
using namespace hkl;

class Mode_Eulerian4C_Test : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( Mode_Eulerian4C_Test );
  
  CPPUNIT_TEST( Bissector );
  CPPUNIT_TEST( Delta_Theta );
  
  CPPUNIT_TEST_SUITE_END();

  Crystal m_crystal;
  Source m_source;
  angleConfiguration::Eulerian4C m_aC_E4C;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Bissector(void);
  void Delta_Theta(void);
};

#endif //_MODE_EULERIAN4C_TEST_H_