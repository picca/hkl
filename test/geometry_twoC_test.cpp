#include "geometry_twoC_test.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "constants.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTwoCTest );

void
GeometryTwoCTest::setUp(void) {}

void 
GeometryTwoCTest::tearDown(void) {}

void
GeometryTwoCTest::equal(void)
{
    CPPUNIT_ASSERT_EQUAL(m_geometry, m_geometry);
}

void 
GeometryTwoCTest::copyConstructor(void)
{
    geometry::twoC::Vertical geometry(m_geometry);

    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry);
}

void 
GeometryTwoCTest::otherConstructors(void)
{
    double omega = 10 * constant::math::degToRad;
    double two_theta =13 * constant::math::degToRad;

    geometry::twoC::Vertical geometry_ref;
    geometry::twoC::Vertical geometry(omega, two_theta);

    geometry_ref.get_axe("omega").set_value(omega);
    geometry_ref.get_axe("2theta").set_value(two_theta);

    CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryTwoCTest::getAxesNames(void)
{
    vector<MyString> v = m_geometry.getAxesNames();
    CPPUNIT_ASSERT_EQUAL(MyString("omega"), v[0]);
    CPPUNIT_ASSERT_EQUAL(MyString("2theta"), v[1]);
}

void
GeometryTwoCTest::getSampleQuaternion(void)
{
    m_geometry.get_axe("omega").set_value(90 * constant::math::degToRad);

    CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), m_geometry.getSampleQuaternion());
}

void
GeometryTwoCTest::getSampleRotationMatrix(void)
{
    m_geometry.get_axe("omega").set_value(90. * constant::math::degToRad);

    smatrix M( 0., 0.,-1.,
               0., 1., 0.,
               1., 0., 0.);

    CPPUNIT_ASSERT_EQUAL(M, m_geometry.getSampleRotationMatrix());
}

void
GeometryTwoCTest::getQ(void)
{
    m_geometry.get_axe("2theta").set_value(0. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry.getQ());

    m_geometry.get_axe("2theta").set_value(45. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), m_geometry.getQ());
}

void
GeometryTwoCTest::getDistance(void)
{
    geometry::twoC::Vertical g1(10 * constant::math::degToRad,
                                40 * constant::math::degToRad);

    geometry::twoC::Vertical g2(11 * constant::math::degToRad,
                                41 * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);

    g2.get_axe("omega").set_value(10 * constant::math::degToRad);
    g2.get_axe("2theta").set_value(40 * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);
}

void
GeometryTwoCTest::setFromGeometry(void)
{
    geometry::twoC::Vertical twoC;
    geometry::twoC::Vertical twoC_ref(10. * constant::math::degToRad,
                                      40. * constant::math::degToRad);

    //eulerian4C::Vertical
    geometry::eulerian4C::Vertical E4CV(10. * constant::math::degToRad,
                                        0. * constant::math::degToRad,
                                        0. * constant::math::degToRad,
                                        40. * constant::math::degToRad);
    twoC.setFromGeometry(E4CV, true);
    CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

    //kappa4C::Vertical
    geometry::kappa4C::Vertical K4CV(50. * constant::math::degToRad,
                                     10. * constant::math::degToRad,
                                     0. * constant::math::degToRad,
                                     0. * constant::math::degToRad,
                                     40. * constant::math::degToRad);
    twoC.setFromGeometry(K4CV, true);
    CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

    //Kappa6C
    geometry::Kappa6C K6C(50. * constant::math::degToRad,
                          0. * constant::math::degToRad,
                          10. * constant::math::degToRad,
                          0. * constant::math::degToRad,
                          0. * constant::math::degToRad,
                          0. * constant::math::degToRad,
                          40. * constant::math::degToRad);
    twoC.setFromGeometry(K6C, true);
    CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

    // exceptions
    // eulerian4C
    E4CV.get_axe("chi").set_value(1.);
    CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(E4CV, true), HKLException);
    // kappa4C
    K4CV.get_axe("kappa").set_value(1.);
    CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K4CV, true), HKLException);
    // kappa6C
    K6C.get_axe("mu").set_value(1.);
    CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), HKLException);
    K6C.get_axe("mu").set_value(0.);
    K6C.get_axe("gamma").set_value(1.);
    CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), HKLException);
    K6C.get_axe("mu").set_value(1.);
    CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), HKLException);
}

void
GeometryTwoCTest::persistanceIO(void)
{
    geometry::twoC::Vertical geometry1;
    geometry::twoC::Vertical geometry2;  
    stringstream flux;

    m_geometry.toStream(flux);
    m_geometry.toStream(flux);  
    geometry1.fromStream(flux);
    geometry2.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry1);
    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry2);
}
