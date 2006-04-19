#include <iostream>
#include "constants.h"
#include "geometry_kappa4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa4CTest );

void
GeometryKappa4CTest::setUp(void)
{
    m_alpha = 50 * constant::math::degToRad;
    m_geometry = new geometry::kappa4C::Vertical(m_alpha);
}

void 
GeometryKappa4CTest::tearDown(void)
{
    delete m_geometry;
}

void
GeometryKappa4CTest::equal(void)
{
    geometry::kappa4C::Vertical geometry(m_alpha);
    CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void 
GeometryKappa4CTest::copyConstructor(void)
{
    geometry::kappa4C::Vertical geometry(*m_geometry);

    CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void 
GeometryKappa4CTest::otherConstructors(void)
{
    double komega = 10 * constant::math::degToRad;
    double kappa = 11 * constant::math::degToRad;
    double kphi = 12 * constant::math::degToRad;
    double two_theta =13 * constant::math::degToRad;

    geometry::kappa4C::Vertical geometry_ref(m_alpha);
    geometry::kappa4C::Vertical geometry(m_alpha, komega, kappa, kphi, two_theta);

    geometry_ref.get_axe("komega").set_value(komega);
    geometry_ref.get_axe("kappa").set_value(kappa);
    geometry_ref.get_axe("kphi").set_value(kphi);
    geometry_ref.get_axe("2theta").set_value(two_theta);

    CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa4CTest::get_alpha(void)
{
    CPPUNIT_ASSERT_EQUAL(m_alpha, m_geometry->get_alpha());
}

void
GeometryKappa4CTest::getAxesNames(void)
{
    vector<MyString> v = m_geometry->getAxesNames();
    CPPUNIT_ASSERT_EQUAL(MyString("komega"), v[0]);
    CPPUNIT_ASSERT_EQUAL(MyString("kappa"), v[1]);
    CPPUNIT_ASSERT_EQUAL(MyString("kphi"), v[2]);
    CPPUNIT_ASSERT_EQUAL(MyString("2theta"), v[3]);
}

void
GeometryKappa4CTest::getSampleQuaternion(void)
{
    m_geometry->get_axe("komega").set_value(90 * constant::math::degToRad);

    CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), m_geometry->getSampleQuaternion());
}

void
GeometryKappa4CTest::getSampleRotationMatrix(void)
{
    m_geometry->get_axe("komega").set_value(90. * constant::math::degToRad);

    smatrix M( 0., 0.,-1.,
               0., 1., 0.,
               1., 0., 0.);

    CPPUNIT_ASSERT_EQUAL(M, m_geometry->getSampleRotationMatrix());
}

void
GeometryKappa4CTest::getQ(void)
{
    m_geometry->get_axe("2theta").set_value(0. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry->getQ());

    m_geometry->get_axe("2theta").set_value(45. * constant::math::degToRad);
    CPPUNIT_ASSERT_EQUAL(svector(1./sqrt(2)-1., 0, 1./sqrt(2.)), m_geometry->getQ());
}

void
GeometryKappa4CTest::getDistance(void)
{
    geometry::kappa4C::Vertical g1(m_alpha,
                                   10 * constant::math::degToRad,
                                   20 * constant::math::degToRad,
                                   30 * constant::math::degToRad,
                                   40 * constant::math::degToRad);

    geometry::kappa4C::Vertical g2(m_alpha,
                                   11 * constant::math::degToRad,
                                   21 * constant::math::degToRad,
                                   31 * constant::math::degToRad,
                                   41 * constant::math::degToRad);

    CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);

    g2.get_axe("komega").set_value(10 * constant::math::degToRad);
    g2.get_axe("kappa").set_value(20 * constant::math::degToRad);
    g2.get_axe("kphi").set_value(30 * constant::math::degToRad);
    g2.get_axe("2theta").set_value(40 * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);
}

void
GeometryKappa4CTest::persistanceIO(void)
{
    geometry::kappa4C::Vertical geometry1(m_alpha);
    geometry::kappa4C::Vertical geometry2(m_alpha);
    stringstream flux;

    m_geometry->get_axe("komega").set_value(2.);
    m_geometry->toStream(flux);
    m_geometry->get_axe("komega").set_value(3.);
    m_geometry->toStream(flux);  
    geometry1.fromStream(flux);
    geometry2.fromStream(flux);

    m_geometry->get_axe("komega").set_value(2.);
    CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
    m_geometry->get_axe("komega").set_value(3.);
    CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
