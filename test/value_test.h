#ifndef _VALUE_TEST_H_
#define _VALUE_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "value.h"

using namespace hkl;

class valueTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( valueTest );
  CPPUNIT_TEST( Constructor );
  CPPUNIT_TEST( Equal );
  CPPUNIT_TEST( CopyConstructor );
  CPPUNIT_TEST( GetSet );
  CPPUNIT_TEST( PlusEqual );
  CPPUNIT_TEST( DivideEqual );

  CPPUNIT_TEST( persistanceIO );
  
  CPPUNIT_TEST_SUITE_END();

  Value m_value;

  public:
  
  void setUp(void);
  void tearDown(void);
  
  void Constructor(void);
  void Equal(void);
  void CopyConstructor(void);
  void GetSet(void);
  void PlusEqual(void);
  void DivideEqual(void);
  void persistanceIO(void);
};

#endif //_VALUE_TEST_H_
