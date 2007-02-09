#include "affinement_simplex_test.h"
#include "sample_monocrystal.h"
#include "reflectionlist.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Affinement_SimplexTest );

void
Affinement_SimplexTest::setUp(void)
{
  // initialisation de la source
  _geometry.get_source().setWaveLength(1.54);

  // initialisation du sample
  _sample = SampleFactory(_geometry).create("test", SAMPLE_MONOCRYSTAL);
  Lattice & lattice = _sample->lattice();
  lattice.a().set_current(1.);
  lattice.b().set_current(5.);
  lattice.c().set_current(4.);
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(90 * constant::math::degToRad);
  lattice.gamma().set_current(90 * constant::math::degToRad);

  // Reflection 1
  _geometry.get_axe("2theta").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("omega").set_current(30.*constant::math::degToRad);
  _geometry.get_axe("chi").set_current(0.*constant::math::degToRad);
  _geometry.get_axe("phi").set_current(90.*constant::math::degToRad);
  _sample->reflections().add(svector(1., 0., 0.));

  // Reflection 2
  _geometry.get_axe("2theta").set_current(60*constant::math::degToRad);
  _geometry.get_axe("omega").set_current(30.*constant::math::degToRad);
  _geometry.get_axe("chi").set_current(90.*constant::math::degToRad);
  _geometry.get_axe("phi").set_current(0.*constant::math::degToRad);
  _sample->reflections().add(svector(0., 1., 0.));

  // Reflection 3
  _geometry.get_axe("2theta").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("omega").set_current(30.*constant::math::degToRad);
  _geometry.get_axe("chi").set_current(0.*constant::math::degToRad);
  _geometry.get_axe("phi").set_current(0.*constant::math::degToRad);
  _sample->reflections().add(svector(0., 0., 1.));

  // Reflection 4
  _geometry.get_axe("2theta").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("omega").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("chi").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("phi").set_current(60.*constant::math::degToRad);
  _sample->reflections().add(svector(0.625, 0.75, -0.216506350946));

  // Reflection 5
  _geometry.get_axe("2theta").set_current(60.*constant::math::degToRad);
  _geometry.get_axe("omega").set_current(45.*constant::math::degToRad);
  _geometry.get_axe("chi").set_current(45.*constant::math::degToRad);
  _geometry.get_axe("phi").set_current(45.*constant::math::degToRad);
  _sample->reflections().add(svector(0.665975615037, 0.683012701892, 0.299950211252));
}

void
Affinement_SimplexTest::tearDown(void)
{
  delete _sample;
}

void
Affinement_SimplexTest::Fit(void)
{
  double a, b, c, alpha, beta, gamma;
  smatrix U_ref(1., 0., 0.,
                0., 1., 0.,
                0., 0., 1.);

  for(unsigned int i=0;i<30;i++)
    {
      CPPUNIT_ASSERT_NO_THROW(_simplex.set_nb_max_iteration(800));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.set_nb_max_iteration(2500));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
    }

  Lattice lattice_ref = Lattice(1.54, 1.54, 1.54,
                                90 * constant::math::degToRad,
                                90 * constant::math::degToRad,
                                90 * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(U_ref, dynamic_cast<hkl::sample::MonoCrystal *>(_sample)->get_U());
  CPPUNIT_ASSERT_EQUAL(lattice_ref, _sample->lattice());
}

void
Affinement_SimplexTest::Fit2(void)
{
  /*
   
      // initialisation de la source
      _geometry.get_source().setWaveLength(1.5418);
   
      //crystal.setLattice(4.81, 8.47, 2.941, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90. * constant::math::degToRad);
      crystal.setLattice(1, 1, 5, 60.*constant::math::degToRad, 80.*constant::math::degToRad, 100. * constant::math::degToRad);
   
      //crystal["a"].set_flagFit(false);
      //crystal["b"].set_flagFit(false);
      //crystal["c"].set_flagFit(false);
      //crystal["alpha"].set_flagFit(false);
      //crystal["beta"].set_flagFit(false);
      //crystal["gamma"].set_flagFit(false);
   
      // Reflection 1
      _geometry.get_axe("2theta").set_value(30.398*constant::math::degToRad);
      _geometry.get_axe("omega").set_value(11.709*constant::math::degToRad);
      _geometry.get_axe("chi").set_value(87.607*constant::math::degToRad);
      _geometry.get_axe("phi").set_value(0.265*constant::math::degToRad);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, 0., 0., 1., 0, true));
   
      // Reflection 2
      _geometry.get_axe("2theta").set_value(21.001*constant::math::degToRad);
      _geometry.get_axe("omega").set_value(10.322*constant::math::degToRad);
      _geometry.get_axe("chi").set_value(-2.139*constant::math::degToRad);
      _geometry.get_axe("phi").set_value(0.023*constant::math::degToRad);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, 0., 2., 0., 0, true));
   
      // Reflection 3
      _geometry.get_axe("2theta").set_value(54.046*constant::math::degToRad);
      _geometry.get_axe("omega").set_value(26.872*constant::math::degToRad);
      _geometry.get_axe("chi").set_value(34.938*constant::math::degToRad);
      _geometry.get_axe("phi").set_value(57.295*constant::math::degToRad);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, -2, 2., 1., 0, true));
   
      // Reflection 4
      _geometry.get_axe("2theta").set_value(37.333*constant::math::degToRad);
      _geometry.get_axe("omega").set_value(18.51*constant::math::degToRad);
      _geometry.get_axe("chi").set_value(53.966*constant::math::degToRad);
      _geometry.get_axe("phi").set_value(54.505*constant::math::degToRad);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, -1, 1., 1., 0, true));
   
      _simplex.set_nb_max_iteration(5000);
      Crystal<geometry::eulerian4C::Vertical> crystal1(crystal);
      _simplex.fit(crystal);
      _simplex.fit(crystal);
      _simplex.fit(crystal);
      _simplex.set_nb_max_iteration(15000);
      _simplex.fit(crystal);
      //std::cout << crystal1;
      //std::cout << crystal;
      //std::cout << crystal.get_U()*crystal.get_B();
  */
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
