#include "reflection_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionTest );

void
ReflectionTest::setUp(void)
{
    m_geometry_E4C = hkl::geometry::eulerian4C::Vertical(1, 2, 3, 1);
    m_geometry_E4C.get_source().setKi(hkl::svector(1., 0., 0.));
}

void 
ReflectionTest::tearDown(void) 
{}

void 
ReflectionTest::Constructor(void)
{
    hkl::Reflection reflection(m_geometry_E4C, 1., 0., 0., hkl::Best, true);

    CPPUNIT_ASSERT_EQUAL(1., reflection.h().get_value());
    CPPUNIT_ASSERT_EQUAL(0., reflection.k().get_value());
    CPPUNIT_ASSERT_EQUAL(0., reflection.l().get_value());
    CPPUNIT_ASSERT_EQUAL((int)hkl::Best, reflection.relevance());
    CPPUNIT_ASSERT_EQUAL(true, reflection.flag());
    CPPUNIT_ASSERT_EQUAL(hkl::MyString("Best"), reflection.getStrRelevance());
}

void 
ReflectionTest::Equal(void)
{ 
    hkl::Reflection r(m_geometry_E4C, 1., 0., 0., hkl::Best, true);
    CPPUNIT_ASSERT_EQUAL(r, r);
}

void
ReflectionTest::GetSet(void)
{
    hkl::Reflection r(m_geometry_E4C, 1., 0., 0., hkl::Best, true);

    r.h().set_value(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.h().get_value());

    r.k().set_value(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.k().get_value());

    r.l().set_value(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.l().get_value());

    r.relevance() = hkl::VerySignificant;
    CPPUNIT_ASSERT_EQUAL((int)hkl::VerySignificant, r.relevance());

    r.flag() = false;
    CPPUNIT_ASSERT_EQUAL(false, r.flag());
}

void
ReflectionTest::GetHKL(void)
{
    hkl::Reflection r(m_geometry_E4C, 1., 0., 0., hkl::Best, true);
    hkl::svector vref(1., 0., 0.);

    CPPUNIT_ASSERT_EQUAL(vref, r.getHKL());
}

void 
ReflectionTest::ComputeAngle(void)
{ 
    double angle;
    const hkl::Reflection r(m_geometry_E4C, 1., 0., 0., hkl::Best, true);
    const hkl::Reflection r1(m_geometry_E4C, 1., 1., .5, hkl::Best, true);  

    angle = r.computeAngle(1., 0., 0.).get_value();
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, hkl::constant::math::epsilon_0);

    angle = r.computeAngle(1., 1., 0.).get_value();
    CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, hkl::constant::math::epsilon_0);

    angle = r1.computeAngle(1, .5, -1.).get_value();
    CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, hkl::constant::math::epsilon_0);
}

void
ReflectionTest::isColinear(void)
{
    hkl::Reflection r(m_geometry_E4C, 1., 0., 0., hkl::Best, true);
    hkl::Reflection r1(m_geometry_E4C, 2., 0., 0., hkl::Best, true);
    hkl::Reflection r2(m_geometry_E4C, 1., 1., .5, hkl::Best, true);  

    CPPUNIT_ASSERT_EQUAL(true, r.isColinear(r));
    CPPUNIT_ASSERT_EQUAL(true, r.isColinear(r1));
    CPPUNIT_ASSERT_EQUAL(false, r.isColinear(r2));
}

void
ReflectionTest::persistanceIO(void)
{
    hkl::Reflection r_ref(m_geometry_E4C, 1., 0., 0., hkl::Best, true);
    hkl::Reflection r1_ref(m_geometry_E4C, 2., 0., 0., hkl::Best, true);
    hkl::Reflection r(m_geometry_E4C, 0, 0, 0, hkl::VerySignificant, true);
    hkl::Reflection r1(m_geometry_E4C, 0, 0, 0, hkl::VerySignificant, true);
    stringstream flux;

    r_ref.toStream(flux);
    r1_ref.toStream(flux);  
    r.fromStream(flux);
    r1.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(r1_ref, r1);
    CPPUNIT_ASSERT_EQUAL(r_ref, r);
}
