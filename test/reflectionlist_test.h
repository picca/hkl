#ifndef _REFLECTIONLIST_TEST_H
#define _REFLECTIONLIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "reflectionlist.h"
#include "eulerian4C_vertical_geometry.h"

using hkl::HKLException;

class ReflectionListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( ReflectionListTest );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( clone );
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( del );
    CPPUNIT_TEST( size );
    CPPUNIT_TEST( size_indep );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::ReflectionList * _reflectionList;
    hkl::eulerian4C::vertical::Geometry _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void operators(void);
    void clone(void);
    void add(void);
    void del(void);
    void size(void);
    void size_indep(void);
    void persistanceIO(void);
  };

#endif /* _REFLECTIONLIST_TEST_H */
