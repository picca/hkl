#ifndef _REFLECTION_TEST_H
#define _REFLECTION_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <string>

#include "source.h"
#include "reflection.h"
#include "eulerian4C_vertical_geometry.h"

using std::cout;
using std::string;

class ReflectionTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( ReflectionTest );
    CPPUNIT_TEST( Constructor );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( GetHKL );
    CPPUNIT_TEST( ComputeAngle );
    CPPUNIT_TEST( isColinear );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::Reflection * _reflections;
    hkl::eulerian4C::vertical::Geometry * _geometry;
    hkl::Source _source;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructor(void);
    void Equal(void);
    void GetSet(void);
    void GetHKL(void);
    void ComputeAngle(void);
    void isColinear(void);
    void persistanceIO(void);
  };

#endif /* _REFLECTION_TEST_H */
