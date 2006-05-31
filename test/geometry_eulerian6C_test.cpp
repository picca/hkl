#include <sstream>
#include "constants.h"
#include "geometry_eulerian6C_test.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "geometry_twoC.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryEulerian6CTest );

void
GeometryEulerian6CTest::setUp(void)
{
    m_geometry = geometry::Eulerian6C();
}

void 
GeometryEulerian6CTest::tearDown(void) {}

void
GeometryEulerian6CTest::equal(void)
{
    CPPUNIT_ASSERT_EQUAL(m_geometry, m_geometry);
}

void 
GeometryEulerian6CTest::copyConstructor(void)
{
    geometry::Eulerian6C geometry(m_geometry);

    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry);
}

void 
GeometryEulerian6CTest::otherConstructors(void)
{
    double mu = 9 * constant::math::degToRad;
    double omega = 10 * constant::math::degToRad;
    double chi = 11 * constant::math::degToRad;
    double phi = 12 * constant::math::degToRad;
    
    double gamma = 13 * constant::math::degToRad;
    double delta = 14 * constant::math::degToRad;

    m_geometry = geometry::Eulerian6C(mu, omega, chi, phi, gamma, delta);

    CPPUNIT_ASSERT_DOUBLES_EQUAL(mu, m_geometry.get_axe("mu").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(chi, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(phi, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(gamma, m_geometry.get_axe("gamma").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(delta, m_geometry.get_axe("delta").get_value(), constant::math::epsilon_0);
}

void 
GeometryEulerian6CTest::setAngles(void)
{
    double mu = 9 * constant::math::degToRad;
    double omega = 10 * constant::math::degToRad;
    double chi = 11 * constant::math::degToRad;
    double phi = 12 * constant::math::degToRad;
    
    double gamma = 13 * constant::math::degToRad;
    double delta = 14 * constant::math::degToRad;

    m_geometry.setAngles(mu, omega, chi, phi, gamma, delta);

    CPPUNIT_ASSERT_DOUBLES_EQUAL(mu, m_geometry.get_axe("mu").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(chi, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(phi, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(gamma, m_geometry.get_axe("gamma").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(delta, m_geometry.get_axe("delta").get_value(), constant::math::epsilon_0);
}

void
GeometryEulerian6CTest::getAxesNames(void)
{
    vector<MyString> v = m_geometry.getAxesNames();
    CPPUNIT_ASSERT_EQUAL(MyString("mu"), v[0]);
    CPPUNIT_ASSERT_EQUAL(MyString("omega"), v[1]);
    CPPUNIT_ASSERT_EQUAL(MyString("chi"), v[2]);
    CPPUNIT_ASSERT_EQUAL(MyString("phi"), v[3]);
    CPPUNIT_ASSERT_EQUAL(MyString("gamma"), v[4]);
    CPPUNIT_ASSERT_EQUAL(MyString("delta"), v[5]);
}

void
GeometryEulerian6CTest::getSampleQuaternion(void)
{
    m_geometry.get_axe("mu").set_value(90 * constant::math::degToRad);

    CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry.getSampleQuaternion());
}

void
GeometryEulerian6CTest::getSampleRotationMatrix(void)
{
    m_geometry.get_axe("mu").set_value(90. * constant::math::degToRad);

    smatrix M( 0.,-1., 0.,
               1., 0., 0.,
               0., 0., 1.);

    CPPUNIT_ASSERT_EQUAL(M, m_geometry.getSampleRotationMatrix());
}

void
GeometryEulerian6CTest::getQ(void)
{
    m_geometry.get_axe("gamma").set_value(0. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(0. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry.getQ());

    m_geometry.get_axe("gamma").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(45. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(-.5, .5, sqrt(2.)/2.), m_geometry.getQ());
}

void
GeometryEulerian6CTest::getKf(void)
{
    m_geometry.get_axe("gamma").set_value(0. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(0. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(1., 0., 0.), m_geometry.getKf());

    m_geometry.get_axe("gamma").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(45. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(.5, .5, sqrt(2.)/2.), m_geometry.getKf());
    
    m_geometry.get_axe("gamma").set_value(-45. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(-45. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(.5, -.5, -sqrt(2.)/2.), m_geometry.getKf());
}

void
GeometryEulerian6CTest::getDistance(void)
{
    geometry::Eulerian6C g1(10 * constant::math::degToRad,
                            20 * constant::math::degToRad,
                            30 * constant::math::degToRad,
                            40 * constant::math::degToRad,
                            50 * constant::math::degToRad,
                            60 * constant::math::degToRad);

    geometry::Eulerian6C g2(11 * constant::math::degToRad,
                            21 * constant::math::degToRad,
                            31 * constant::math::degToRad,
                            41 * constant::math::degToRad,
                            51 * constant::math::degToRad,
                            61 * constant::math::degToRad);
    
    CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);

    g2.get_axe("mu").set_value(10 * constant::math::degToRad);
    g2.get_axe("omega").set_value(20 * constant::math::degToRad);
    g2.get_axe("chi").set_value(30 * constant::math::degToRad);
    g2.get_axe("phi").set_value(40 * constant::math::degToRad);
    g2.get_axe("gamma").set_value(50 * constant::math::degToRad);
    g2.get_axe("delta").set_value(60 * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);
}

void
GeometryEulerian6CTest::setFromGeometry(void)
{
    geometry::Eulerian6C E6C;
    geometry::Eulerian6C E6C_ref(0. * constant::math::degToRad,
                                 100. * constant::math::degToRad,
                                 0. * constant::math::degToRad,
                                 -90. * constant::math::degToRad,
                                 0. * constant::math::degToRad,
                                 40. * constant::math::degToRad);
    //eulerian4C::Vertical
    geometry::eulerian4C::Vertical E4CV(100. * constant::math::degToRad,
                                        0. * constant::math::degToRad,
                                        -90. * constant::math::degToRad,
                                        40. * constant::math::degToRad);
    E6C.setFromGeometry(E4CV, true);
    CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

    //kappa4C::Vertical
    geometry::kappa4C::Vertical K4CV(50. * constant::math::degToRad,
                                     10. * constant::math::degToRad,
                                     0. * constant::math::degToRad,
                                     0. * constant::math::degToRad,
                                     40. * constant::math::degToRad);
    E6C.setFromGeometry(K4CV, true);
    CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

    //Kappa6C
    geometry::Kappa6C K6C(50. * constant::math::degToRad);
    E6C.setFromGeometry(K6C, true);
    E6C_ref.get_axe("omega").set_value(90 * constant::math::degToRad);
    E6C_ref.get_axe("delta").set_value(0 * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);
}

void
GeometryEulerian6CTest::persistanceIO(void)
{
    geometry::Eulerian6C geometry1;
    geometry::Eulerian6C geometry2;  
    stringstream flux;

    m_geometry.toStream(flux);
    m_geometry.toStream(flux);  
    geometry1.fromStream(flux);
    geometry2.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry1);
    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry2);
}
