#include "reflectionlistfactory_test.h"
#include "reflectionlistfactory_monocrystal.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionListFactoryTest );

void
ReflectionListFactoryTest::setUp(void)
{
    _reflectionListFactory = new hkl::reflectionlistfactory::MonoCrystal(_geometry);
    _reflectionListFactory->add(1, 1, 1);
    _reflectionListFactory->add(1, 1, 1);
}

void 
ReflectionListFactoryTest::tearDown(void) 
{
    delete _reflectionListFactory;
}

void 
ReflectionListFactoryTest::constructors(void)
{
    hkl::ReflectionListFactory * factory = _reflectionListFactory->clone();
    CPPUNIT_ASSERT_EQUAL(*_reflectionListFactory, *factory);
    delete factory;
}

void
ReflectionListFactoryTest::persistanceIO(void)
{
    hkl::ReflectionListFactory * reflectionListFactory = new hkl::reflectionlistfactory::MonoCrystal(_geometry);
    stringstream flux;

    _reflectionListFactory->toStream(flux);
    reflectionListFactory->fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(*_reflectionListFactory, *reflectionListFactory);
    delete reflectionListFactory;
}
