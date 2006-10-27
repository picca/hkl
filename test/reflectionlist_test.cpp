#include "reflectionlist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionListTest );

void
ReflectionListTest::setUp(void)
{
    _reflectionList = new hkl::ReflectionList(_geometry, REFLECTION_MONOCRYSTAL);
}

void 
ReflectionListTest::tearDown(void) 
{
    delete _reflectionList;
}

void
ReflectionListTest::operators(void)
{
  // ==
  CPPUNIT_ASSERT_EQUAL(_reflectionList, _reflectionList);
}

void 
ReflectionListTest::clone(void)
{
    _reflectionList->add(1, 1, 1);
    _reflectionList->add(1, 1, 1);
    hkl::ReflectionList * factory = _reflectionList->clone();
    CPPUNIT_ASSERT_EQUAL(*_reflectionList, *factory);
    delete factory;
}

void 
ReflectionListTest::add(void)
{
    // add 2 timesd the last reflection and test
    // if the last reflection have the flag set to false.
    // we can not have two identical reflection (h1, k1, l1) == (h2, k2, l2) active
    // for calculation.
    _reflectionList->add(1, 1, 1);
    _reflectionList->add(1, 1, 1);
    CPPUNIT_ASSERT_EQUAL(true, (*_reflectionList)[0].flag());
    CPPUNIT_ASSERT_EQUAL(false, (*_reflectionList)[1].flag());
}

void
ReflectionListTest::persistanceIO(void)
{
    hkl::ReflectionList * reflectionList = new hkl::ReflectionList(_geometry, REFLECTION_MONOCRYSTAL);
    stringstream flux;

    _reflectionList->toStream(flux);
    reflectionList->fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(*_reflectionList, *reflectionList);
    delete reflectionList;
}
