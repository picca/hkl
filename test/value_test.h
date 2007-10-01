#ifndef _VALUE_TEST_H_
#define _VALUE_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "value.h"

using namespace hkl;

class valueTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( valueTest );

    CPPUNIT_TEST( Constructors );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( Comparisons );
    CPPUNIT_TEST( PlusEqual );
    CPPUNIT_TEST( DivideEqual );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( fabs );

    CPPUNIT_TEST_SUITE_END();

    Value m_value;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructors(void);
    void GetSet(void);
    void Comparisons(void);
    void PlusEqual(void);
    void DivideEqual(void);
    void operators(void);
    void fabs(void);
  };

#endif //_VALUE_TEST_H_
