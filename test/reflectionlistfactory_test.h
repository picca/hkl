#ifndef _REFLECTIONLISTFACTORY_TEST_H
#define _REFLECTIONLISTFACTORY_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "reflectionlistfactory.h"
#include "geometry_eulerian4C.h"

class ReflectionListFactoryTest : public CppUnit::TestFixture  {
  CPPUNIT_TEST_SUITE( ReflectionListFactoryTest );
  CPPUNIT_TEST( constructors );
  CPPUNIT_TEST( persistanceIO );

  CPPUNIT_TEST_SUITE_END();

  hkl::ReflectionListFactory * _reflectionListFactory;
  hkl::geometry::eulerian4C::Vertical _geometry;
  
  public:
  
  void setUp(void);
  void tearDown(void);
  
  void constructors(void);
  void persistanceIO(void);
};

#endif /* _REFLECTIONLISTFACTORY_TEST_H */
