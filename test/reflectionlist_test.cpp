#include "reflectionlist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionListTest );

void
ReflectionListTest::setUp(void)
{
    _reflectionList = new hkl::ReflectionList(_geometry, hkl::REFLECTION_MONOCRYSTAL);
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

    // []
    CPPUNIT_ASSERT_THROW((*_reflectionList)[0], HKLException);
    // throw if the geometry is not valid.
    CPPUNIT_ASSERT_THROW(_reflectionList->add(hkl::svector(1, 1, 1)), HKLException);
    _geometry.get_source().setWaveLength(1.54);
    // no throw if the source of the geometry is correct.
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_NO_THROW((*_reflectionList)[0]);
    CPPUNIT_ASSERT_THROW((*_reflectionList)[1], HKLException);
}

void 
ReflectionListTest::clone(void)
{
    _geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    hkl::ReflectionList * factory;
    CPPUNIT_ASSERT_NO_THROW( factory = _reflectionList->clone());
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
    _geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_EQUAL(true, (*_reflectionList)[0]->flag());
    CPPUNIT_ASSERT_EQUAL(false, (*_reflectionList)[1]->flag());
}

void 
ReflectionListTest::del(void)
{
    // add 2 timesd the last reflection and test
    // if deletion is ok.
    _geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->del(0));
    CPPUNIT_ASSERT_THROW(_reflectionList->del(1), HKLException);
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->del(0));
}

void
ReflectionListTest::size(void)
{
    _geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_EQUAL((unsigned int)0, _reflectionList->size());
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size());
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_EQUAL((unsigned int)2, _reflectionList->size());
}

void
ReflectionListTest::size_indep(void)
{
    _geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_EQUAL((unsigned int)0, _reflectionList->size_indep());
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size_indep());
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
    CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size_indep());
    CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 0, 1)));
    CPPUNIT_ASSERT_EQUAL((unsigned int)2, _reflectionList->size_indep());
}

void
ReflectionListTest::persistanceIO(void)
{
    hkl::ReflectionList * reflectionList = new hkl::ReflectionList(_geometry, hkl::REFLECTION_MONOCRYSTAL);
    stringstream flux;

    _reflectionList->toStream(flux);
    reflectionList->fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(*_reflectionList, *reflectionList);
    delete reflectionList;
}
