#include "reflection_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionTest );

void
ReflectionTest::setUp(void)
{
    m_geometry_E4C = geometry::eulerian4C::Vertical(1, 2, 3, 1);
    m_geometry_E4C.get_source().setKi(svector(1., 0., 0.));
}

void 
ReflectionTest::tearDown(void) 
{}

void 
ReflectionTest::Constructor(void)
{
    Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);

    CPPUNIT_ASSERT_EQUAL(1., r.get_h());
    CPPUNIT_ASSERT_EQUAL(0., r.get_k());
    CPPUNIT_ASSERT_EQUAL(0., r.get_l());
    CPPUNIT_ASSERT_EQUAL((int)Best, r.get_relevance());
    CPPUNIT_ASSERT_EQUAL(true, r.get_flag());
    CPPUNIT_ASSERT_EQUAL(MyString("Best"), r.getStrRelevance());
}

void 
ReflectionTest::Equal(void)
{ 
    Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);
    CPPUNIT_ASSERT_EQUAL(r, r);
}

void
ReflectionTest::GetSet(void)
{
    Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);

    r.set_h(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.get_h());

    r.set_k(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.get_k());

    r.set_l(1.5);
    CPPUNIT_ASSERT_EQUAL(1.5, r.get_l());

    r.set_relevance(VerySignificant);
    CPPUNIT_ASSERT_EQUAL((int)VerySignificant, r.get_relevance());

    r.set_flag(false);
    CPPUNIT_ASSERT_EQUAL(false, r.get_flag());
}

void
ReflectionTest::GetHKL(void)
{
    Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);
    svector vref(1., 0., 0.);

    CPPUNIT_ASSERT_EQUAL(vref, r.getHKL());
}

void 
ReflectionTest::ComputeAngle(void)
{ 
    double angle;
    const Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);
    const Reflection<geometry::eulerian4C::Vertical> r1(m_geometry_E4C, 1., 1., .5, Best, true);  

    angle = r.computeAngle(1., 0., 0.);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, constant::math::epsilon_0);

    angle = r.computeAngle(1., 1., 0.);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, constant::math::epsilon_0);

    angle = r1.computeAngle(1, .5, -1.);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, constant::math::epsilon_0);
}

void
ReflectionTest::isColinear(void)
{
    Reflection<geometry::eulerian4C::Vertical> r(m_geometry_E4C, 1., 0., 0., Best, true);
    Reflection<geometry::eulerian4C::Vertical> r1(m_geometry_E4C, 2., 0., 0., Best, true);
    Reflection<geometry::eulerian4C::Vertical> r2(m_geometry_E4C, 1., 1., .5, Best, true);  

    CPPUNIT_ASSERT_EQUAL(true, r.isColinear(r));
    CPPUNIT_ASSERT_EQUAL(true, r.isColinear(r1));
    CPPUNIT_ASSERT_EQUAL(false, r.isColinear(r2));
}

void
ReflectionTest::persistanceIO(void)
{
    Reflection<geometry::eulerian4C::Vertical> r_ref(m_geometry_E4C, 1., 0., 0., Best, true);
    Reflection<geometry::eulerian4C::Vertical> r1_ref(m_geometry_E4C, 2., 0., 0., Best, true);
    Reflection<geometry::eulerian4C::Vertical> r, r1;
    stringstream flux;

    r_ref.toStream(flux);
    r1_ref.toStream(flux);  
    r.fromStream(flux);
    r1.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(r1_ref, r1);
    CPPUNIT_ASSERT_EQUAL(r_ref, r);
}
