#include "affinement_simplex_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Affinement_SimplexTest );

void
Affinement_SimplexTest::setUp(void)
{
  // initialisation de la source
  m_geometry_E4C.get_source().setWaveLength(1.54);

  m_crystal.setLattice(1., 5., 4., 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90. * constant::math::degToRad);
 
  // Reflection 1
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(90.*constant::math::degToRad);
  m_crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 1., 0., 0., 0, true));
  
  // Reflection 2
  m_geometry_E4C.get_axe("2theta").set_value(60*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(90.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.*constant::math::degToRad);
  m_crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0., 1., 0., 0, true));

  // Reflection 3
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.*constant::math::degToRad);
  m_crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0., 0., 1., 0, true));

  // Reflection 4
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(60.*constant::math::degToRad);
  m_crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0.625, 0.75, -0.216506350946, 0, true));
  
  // Reflection 5
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(45.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(45.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(45.*constant::math::degToRad);
  m_crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0.665975615037, 0.683012701892, 0.299950211252, 0, true));
}

void 
Affinement_SimplexTest::tearDown(void) {}

void
Affinement_SimplexTest::Fit(void)
{
  double a, b, c, alpha, beta, gamma;
  smatrix U(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);
 
  for(unsigned int i=0;i<4;i++)
    {
      m_simplex.set_nb_max_iteration(800);
      m_simplex.fit(m_crystal);
      m_simplex.fit(m_crystal);
      m_simplex.fit(m_crystal);
      m_simplex.set_nb_max_iteration(2500);
      m_simplex.fit(m_crystal);
    }

  m_crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, gamma, constant::math::epsilon_0);
  CPPUNIT_ASSERT_EQUAL(U, m_crystal.get_U());
}

void
Affinement_SimplexTest::Fit2(void)
{
    Crystal<geometry::eulerian4C::Vertical> crystal;

    // initialisation de la source
    m_geometry_E4C.get_source().setWaveLength(1.5418);

    //crystal.setLattice(4.81, 8.47, 2.941, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90. * constant::math::degToRad);
    crystal.setLattice(1, 1, 5, 60.*constant::math::degToRad, 80.*constant::math::degToRad, 100. * constant::math::degToRad);

    //crystal["a"].set_flagFit(false);
    //crystal["b"].set_flagFit(false);
    //crystal["c"].set_flagFit(false);
    //crystal["alpha"].set_flagFit(false);
    //crystal["beta"].set_flagFit(false);
    //crystal["gamma"].set_flagFit(false);

    // Reflection 1
    m_geometry_E4C.get_axe("2theta").set_value(30.398*constant::math::degToRad);
    m_geometry_E4C.get_axe("omega").set_value(11.709*constant::math::degToRad);
    m_geometry_E4C.get_axe("chi").set_value(87.607*constant::math::degToRad);
    m_geometry_E4C.get_axe("phi").set_value(0.265*constant::math::degToRad);
    crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0., 0., 1., 0, true));

    // Reflection 2
    m_geometry_E4C.get_axe("2theta").set_value(21.001*constant::math::degToRad);
    m_geometry_E4C.get_axe("omega").set_value(10.322*constant::math::degToRad);
    m_geometry_E4C.get_axe("chi").set_value(-2.139*constant::math::degToRad);
    m_geometry_E4C.get_axe("phi").set_value(0.023*constant::math::degToRad);
    crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, 0., 2., 0., 0, true));

    // Reflection 3
    m_geometry_E4C.get_axe("2theta").set_value(54.046*constant::math::degToRad);
    m_geometry_E4C.get_axe("omega").set_value(26.872*constant::math::degToRad);
    m_geometry_E4C.get_axe("chi").set_value(34.938*constant::math::degToRad);
    m_geometry_E4C.get_axe("phi").set_value(57.295*constant::math::degToRad);
    crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, -2, 2., 1., 0, true));

    // Reflection 4
    m_geometry_E4C.get_axe("2theta").set_value(37.333*constant::math::degToRad);
    m_geometry_E4C.get_axe("omega").set_value(18.51*constant::math::degToRad);
    m_geometry_E4C.get_axe("chi").set_value(53.966*constant::math::degToRad);
    m_geometry_E4C.get_axe("phi").set_value(54.505*constant::math::degToRad);
    crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C, -1, 1., 1., 0, true));

    m_simplex.set_nb_max_iteration(5000);
    Crystal<geometry::eulerian4C::Vertical> crystal1(crystal);
    m_simplex.fit(crystal);
    m_simplex.fit(crystal);
    m_simplex.fit(crystal);
    m_simplex.set_nb_max_iteration(15000);
    m_simplex.fit(crystal);
    //std::cout << crystal1;
    //std::cout << crystal;
    //std::cout << crystal.get_U()*crystal.get_B();
}

void
Affinement_SimplexTest::persistanceIO(void)
{
    hkl::affinement::Simplex simplex_ref, simplex;
    hkl::affinement::Simplex simplex1_ref, simplex1;
    stringstream flux;

    // Modification of the default parameters to be sure
    // that serialization is ok.
    simplex_ref.set_nb_max_iteration(1500);

    simplex_ref.toStream(flux);
    simplex1_ref.toStream(flux);
    simplex.fromStream(flux);
    simplex1.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(simplex_ref, simplex);
    CPPUNIT_ASSERT_EQUAL(simplex1_ref, simplex1);
}
