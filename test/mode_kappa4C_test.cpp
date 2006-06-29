#include "mode_kappa4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Kappa4C_Test );

void
Mode_Kappa4C_Test::setUp(void)
{
    double alpha = 50. * constant::math::degToRad;
    geometry::eulerian4C::Vertical geometry;
    
    geometry.get_source().setWaveLength(1.54);
    m_crystal.setLattice(1.54, 1.54, 1.54, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad);
    

    geometry.get_axe("omega").set_value(30.*constant::math::degToRad);
    geometry.get_axe("chi").set_value(0.*constant::math::degToRad);
    geometry.get_axe("phi").set_value(90.*constant::math::degToRad);
    geometry.get_axe("2theta").set_value(60.*constant::math::degToRad);
    m_geometry.setFromGeometry(geometry, true);
    m_crystal.addReflection(Reflection<geometry::kappa4C::Vertical>(m_geometry,
                                                                    1., 0., 0.,
                                                                    Best, true));

    geometry.get_axe("omega").set_value(30.*constant::math::degToRad);
    geometry.get_axe("chi").set_value(0.*constant::math::degToRad);
    geometry.get_axe("phi").set_value(180.*constant::math::degToRad);
    geometry.get_axe("2theta").set_value(60.*constant::math::degToRad);
    m_geometry.setFromGeometry(geometry, true);
    m_crystal.addReflection(Reflection<geometry::kappa4C::Vertical>(m_geometry,
                                                                    0., 1., 0.,
                                                                    Best, true));

    m_crystal.computeU();
}

void 
Mode_Kappa4C_Test::tearDown(void) {}

void 
Mode_Kappa4C_Test::Bissector(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();

    mode::kappa4C::vertical::eulerian4C::Bissector mode;

    mode.computeAngles(1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-60. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(180. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(-1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-60. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-60. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(270. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0.,-1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-60. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 90. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 0., 1., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  -2.95484 * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-134.755927 * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 147.045165 * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);  
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 0., -1., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-117.045165 * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 134.755927 * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  32.9548353 * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);  
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(1., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( -45. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(225. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);  
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 90. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    // random test
    double h, k, l;
    for(unsigned int i=0;i<1000;i++)
      {
        double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        try
          {
            mode.computeAngles(h0, k0, l0, UB, m_geometry);
            m_geometry.computeHKL(h, k, l, UB);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon_0);
          }
        catch (HKLException)
          {}
      }
}

void 
Mode_Kappa4C_Test::Delta_Theta(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();

    mode::kappa4C::vertical::eulerian4C::Delta_Theta mode;
    mode.setParameterValue("delta theta", 10 * constant::math::degToRad);

    mode.computeAngles(-1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-50. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-10. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-50. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(260. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0.,-1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-50. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 80. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 60. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    mode.computeAngles(1., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-35. * constant::math::degToRad, m_geometry.get_axe("komega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(  0. * constant::math::degToRad, m_geometry.get_axe("kappa").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(215. * constant::math::degToRad, m_geometry.get_axe("kphi").get_value(), constant::math::epsilon_0);  
    CPPUNIT_ASSERT_DOUBLES_EQUAL( 90. * constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);

    CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 1., UB, m_geometry), HKLException);

    // random test
    double h, k, l;
    for(unsigned int i=0;i<1000;i++)
      {
        double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        try
          {
            mode.computeAngles(h0, k0, l0, UB, m_geometry);
            m_geometry.computeHKL(h, k, l, UB);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon_0);
          }
        catch (HKLException)
          {}
      }
}

void 
Mode_Kappa4C_Test::Constant_Omega(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::kappa4C::vertical::eulerian4C::Constant_Omega mode;
    double h, k, l;

    for(unsigned int i=0;i<1000;i++)
      {
        double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        try
          {
            mode.computeAngles(h0, k0, l0, UB, m_geometry);
            m_geometry.computeHKL(h, k, l, UB);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon_0);
          }
        catch (HKLException)
          {}
      }
}

void 
Mode_Kappa4C_Test::Constant_Chi(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::kappa4C::vertical::eulerian4C::Constant_Chi mode;
    double h, k, l;

    for(unsigned int i=0;i<1000;i++)
      {
        double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        try
          {
            mode.computeAngles(h0, k0, l0, UB, m_geometry);
            m_geometry.computeHKL(h, k, l, UB);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon_0);
          }
        catch (HKLException)
          {}
      }
}

void 
Mode_Kappa4C_Test::Constant_Phi(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::kappa4C::vertical::eulerian4C::Constant_Phi mode;
    double h, k, l;

    for(unsigned int i=0;i<1000;i++)
      {
        double h0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double k0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        double l0 = 4. * rand() / (RAND_MAX + 1.) - 2.;
        try
          {
            mode.computeAngles(h0, k0, l0, UB, m_geometry);
            m_geometry.computeHKL(h, k, l, UB);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(h0, h, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(k0, k, constant::math::epsilon_0);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(l0, l, constant::math::epsilon_0);
          }
        catch (HKLException)
          {}
      }
}

void
Mode_Kappa4C_Test::persistanceIO(void)
{
    mode::kappa4C::vertical::eulerian4C::Bissector bissector_ref, bissector;
    mode::kappa4C::vertical::eulerian4C::Delta_Theta delta_theta_ref, delta_theta;
    mode::kappa4C::vertical::eulerian4C::Constant_Omega constant_omega_ref, constant_omega;
    mode::kappa4C::vertical::eulerian4C::Constant_Chi constant_chi_ref, constant_chi;
    mode::kappa4C::vertical::eulerian4C::Constant_Phi constant_phi_ref, constant_phi;
    stringstream flux;

    bissector_ref.toStream(flux);
    delta_theta_ref.toStream(flux);
    constant_omega_ref.toStream(flux);
    constant_chi_ref.toStream(flux);
    constant_phi_ref.toStream(flux);
    bissector.fromStream(flux);
    delta_theta.fromStream(flux);
    constant_omega.fromStream(flux);
    constant_chi.fromStream(flux);
    constant_phi.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(bissector_ref, bissector);
    CPPUNIT_ASSERT_EQUAL(delta_theta_ref, delta_theta);
    CPPUNIT_ASSERT_EQUAL(constant_omega_ref, constant_omega);
    CPPUNIT_ASSERT_EQUAL(constant_chi_ref, constant_chi);
    CPPUNIT_ASSERT_EQUAL(constant_phi_ref, constant_phi);
}
