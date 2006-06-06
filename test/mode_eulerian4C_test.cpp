#include "mode_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Eulerian4C_Test );

void
Mode_Eulerian4C_Test::setUp(void)
{
    m_geometry.get_source().setWaveLength(1.54);

    m_crystal.setLattice(1.54, 1.54, 1.54, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad);

    m_geometry.get_axe("2theta").set_value(60.*constant::math::degToRad);  
    m_geometry.get_axe("omega").set_value(30.*constant::math::degToRad);
    m_geometry.get_axe("chi").set_value(0.*constant::math::degToRad);
    m_geometry.get_axe("phi").set_value(90.*constant::math::degToRad);
    m_crystal.addReflection(Reflection(m_geometry,
                                       1., 0., 0.,
                                       Reflection::Best, true));

    m_geometry.get_axe("phi").set_value(180.*constant::math::degToRad);
    m_crystal.addReflection(Reflection(m_geometry,
                                       0., 1., 0.,
                                       Reflection::Best, true));

    m_crystal.computeU();
}

void 
Mode_Eulerian4C_Test::tearDown(void) {}

void 
Mode_Eulerian4C_Test::Bissector(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();

    mode::eulerian4C::vertical::Bissector mode;

    mode.computeAngles(1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(-1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(180.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0.,-1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 0., 1., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);  

    mode.computeAngles(0., 0., -1., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);  

    mode.computeAngles(1., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(135.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);  

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
Mode_Eulerian4C_Test::Delta_Theta(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();

    mode::eulerian4C::vertical::Delta_Theta mode;
    mode.setParameterValue("delta theta", 10 * constant::math::degToRad);

    mode.computeAngles(-1., 0., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-100*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(170.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(0.,-1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-10.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);

    mode.computeAngles(1., 1., 0., UB, m_geometry);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(55*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(125.*constant::math::degToRad, m_geometry.get_axe("phi").get_value(), constant::math::epsilon_0);  

    //CPPUNIT_ASSERT_THROW( mode.computeAngles(0., 0., 1., UB, m_geometry), HKLException);

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
Mode_Eulerian4C_Test::Constant_Omega(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::eulerian4C::vertical::Constant_Omega mode;
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
Mode_Eulerian4C_Test::Constant_Chi(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::eulerian4C::vertical::Constant_Chi mode;
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
Mode_Eulerian4C_Test::Constant_Phi(void)
{
    smatrix UB = m_crystal.get_U() * m_crystal.get_B();
    mode::eulerian4C::vertical::Constant_Phi mode;
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
Mode_Eulerian4C_Test::persistanceIO(void)
{
    mode::eulerian4C::vertical::Bissector bissector_ref, bissector;
    mode::eulerian4C::vertical::Delta_Theta delta_theta_ref, delta_theta;
    mode::eulerian4C::vertical::Constant_Omega constant_omega_ref, constant_omega;
    mode::eulerian4C::vertical::Constant_Chi constant_chi_ref, constant_chi;
    mode::eulerian4C::vertical::Constant_Phi constant_phi_ref, constant_phi;
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
