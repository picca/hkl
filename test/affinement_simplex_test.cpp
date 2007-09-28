#include "affinement_simplex_test.h"
#include "sample_monocrystal.h"
#include "reflectionlist.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Affinement_SimplexTest );

void
Affinement_SimplexTest::setUp(void)
{
  hkl_svector hkl;

  // initialisation de la source
  _geometry = new hkl::eulerian4C::vertical::Geometry;
  _geometry->get_source().setWaveLength(1.54);

  // initialisation du sample
  _sample = hkl::SampleFactory(*_geometry).create("test", hkl::SAMPLE_MONOCRYSTAL);
  hkl::Lattice & lattice = _sample->lattice();
  lattice.a().set_current(1.);
  lattice.b().set_current(5.);
  lattice.c().set_current(4.);
  lattice.alpha().set_current(90 * HKL_DEGTORAD);
  lattice.beta().set_current(90 * HKL_DEGTORAD);
  lattice.gamma().set_current(90 * HKL_DEGTORAD);

  // Reflection 1
  _geometry->get_axe("tth")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("omega")->set_current(30.*HKL_DEGTORAD);
  _geometry->get_axe("chi")->set_current(0.*HKL_DEGTORAD);
  _geometry->get_axe("phi")->set_current(90.*HKL_DEGTORAD);
  hkl.data[X] = 1;
  hkl.data[Y] = 0;
  hkl.data[Z] = 0;
  _sample->reflections().add(&hkl);

  // Reflection 2
  _geometry->get_axe("tth")->set_current(60*HKL_DEGTORAD);
  _geometry->get_axe("omega")->set_current(30.*HKL_DEGTORAD);
  _geometry->get_axe("chi")->set_current(90.*HKL_DEGTORAD);
  _geometry->get_axe("phi")->set_current(0.*HKL_DEGTORAD);
  hkl.data[X] = 0;
  hkl.data[Y] = 1;
  hkl.data[Z] = 0;
  _sample->reflections().add(&hkl);

  // Reflection 3
  _geometry->get_axe("tth")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("omega")->set_current(30.*HKL_DEGTORAD);
  _geometry->get_axe("chi")->set_current(0.*HKL_DEGTORAD);
  _geometry->get_axe("phi")->set_current(0.*HKL_DEGTORAD);
  hkl.data[X] = 0;
  hkl.data[Y] = 0;
  hkl.data[Z] = 1;
  _sample->reflections().add(&hkl);

  // Reflection 4
  _geometry->get_axe("tth")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("omega")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("chi")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("phi")->set_current(60.*HKL_DEGTORAD);
  hkl.data[X] =  .625;
  hkl.data[Y] =  .75;
  hkl.data[Z] = -.216506350946;
  _sample->reflections().add(&hkl);

  // Reflection 5
  _geometry->get_axe("tth")->set_current(60.*HKL_DEGTORAD);
  _geometry->get_axe("omega")->set_current(45.*HKL_DEGTORAD);
  _geometry->get_axe("chi")->set_current(45.*HKL_DEGTORAD);
  _geometry->get_axe("phi")->set_current(45.*HKL_DEGTORAD);
  hkl.data[X] = .665975615037;
  hkl.data[Y] = .683012701892;
  hkl.data[Z] = .299950211252;
  _sample->reflections().add(&hkl);
}

void
Affinement_SimplexTest::tearDown(void)
{
  delete _sample;
  delete _geometry;
}

void
Affinement_SimplexTest::Fit(void)
{
  static hkl_smatrix U_ref = {{{1., 0., 0.},{0., 1., 0.},{0., 0., 1.}}};

  for (unsigned int i=0;i<30;i++)
    {
      CPPUNIT_ASSERT_NO_THROW(_simplex.set_nb_max_iterations(800));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
      CPPUNIT_ASSERT_NO_THROW(_simplex.set_nb_max_iterations(2500));
      CPPUNIT_ASSERT_NO_THROW(_simplex.fit(*_sample));
    }

  hkl::Lattice lattice_ref = hkl::Lattice(1.54, 1.54, 1.54,
                                          90 * HKL_DEGTORAD,
                                          90 * HKL_DEGTORAD,
                                          90 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&U_ref, dynamic_cast<hkl::sample::MonoCrystal *>(_sample)->get_U()));
  CPPUNIT_ASSERT_EQUAL(lattice_ref, _sample->lattice());
}

void
Affinement_SimplexTest::Fit2(void)
{
  /*
   
      // initialisation de la source
      _geometry->get_source().setWaveLength(1.5418);
   
      //crystal.setLattice(4.81, 8.47, 2.941, 90.*HKL_DEGTORAD, 90.*HKL_DEGTORAD, 90. * HKL_DEGTORAD);
      crystal.setLattice(1, 1, 5, 60.*HKL_DEGTORAD, 80.*HKL_DEGTORAD, 100. * HKL_DEGTORAD);
   
      //crystal["a"].set_flagFit(false);
      //crystal["b"].set_flagFit(false);
      //crystal["c"].set_flagFit(false);
      //crystal["alpha"].set_flagFit(false);
      //crystal["beta"].set_flagFit(false);
      //crystal["gamma"].set_flagFit(false);
   
      // Reflection 1
      _geometry->get_axe("tth").set_value(30.398*HKL_DEGTORAD);
      _geometry->get_axe("omega").set_value(11.709*HKL_DEGTORAD);
      _geometry->get_axe("chi").set_value(87.607*HKL_DEGTORAD);
      _geometry->get_axe("phi").set_value(0.265*HKL_DEGTORAD);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, 0., 0., 1., 0, true));
   
      // Reflection 2
      _geometry->get_axe("tth").set_value(21.001*HKL_DEGTORAD);
      _geometry->get_axe("omega").set_value(10.322*HKL_DEGTORAD);
      _geometry->get_axe("chi").set_value(-2.139*HKL_DEGTORAD);
      _geometry->get_axe("phi").set_value(0.023*HKL_DEGTORAD);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, 0., 2., 0., 0, true));
   
      // Reflection 3
      _geometry->get_axe("tth").set_value(54.046*HKL_DEGTORAD);
      _geometry->get_axe("omega").set_value(26.872*HKL_DEGTORAD);
      _geometry->get_axe("chi").set_value(34.938*HKL_DEGTORAD);
      _geometry->get_axe("phi").set_value(57.295*HKL_DEGTORAD);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, -2, 2., 1., 0, true));
   
      // Reflection 4
      _geometry->get_axe("tth").set_value(37.333*HKL_DEGTORAD);
      _geometry->get_axe("omega").set_value(18.51*HKL_DEGTORAD);
      _geometry->get_axe("chi").set_value(53.966*HKL_DEGTORAD);
      _geometry->get_axe("phi").set_value(54.505*HKL_DEGTORAD);
      crystal.addReflection(Reflection<geometry::eulerian4C::Vertical>(_geometry, -1, 1., 1., 0, true));
   
      _simplex.set_nb_max_iterations(5000);
      Crystal<geometry::eulerian4C::Vertical> crystal1(crystal);
      _simplex.fit(crystal);
      _simplex.fit(crystal);
      _simplex.fit(crystal);
      _simplex.set_nb_max_iterations(15000);
      _simplex.fit(crystal);
      //std::cout << crystal1;
      //std::cout << crystal;
      //std::cout << crystal.get_U()*crystal.get_B();
  */
}
