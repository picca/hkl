#ifndef _REFLECTIONLIST_TEST_H
#define _REFLECTIONLIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "reflectionlist.h"
#include "geometry_eulerian4C.h"

class ReflectionListTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( ReflectionListTest );
  CPPUNIT_TEST( operators );
  CPPUNIT_TEST( clone );
  CPPUNIT_TEST( add );
  CPPUNIT_TEST( persistanceIO );

  CPPUNIT_TEST_SUITE_END();

  hkl::ReflectionList * _reflectionList;
  hkl::geometry::eulerian4C::Vertical _geometry;
  
  public:
  
  void setUp(void);
  void tearDown(void);
  
  void operators(void);
  void clone(void);
  void add(void);
  void persistanceIO(void);
};

#endif /* _REFLECTIONLIST_TEST_H */
