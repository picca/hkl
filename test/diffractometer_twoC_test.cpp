#include "diffractometer_twoC_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Diffractometer_TwoC_Test );

void
Diffractometer_TwoC_Test::setUp(void)
{
  _diffractometer = new hkl::twoC::vertical::Diffractometer();
}

void
Diffractometer_TwoC_Test::tearDown(void)
{
  delete _diffractometer;
}

/*
void
Diffractometer_TwoC_Test::Geometry(void)
{
  hkl::Geometry * geometry = _diffractometer->geometry();

  CPPUNIT_ASSERT_THROW(geometry->get_axe("titi"), HKLException);
  CPPUNIT_ASSERT_THROW(geometry->get_axe("nu"), HKLException);

  CPPUNIT_ASSERT_NO_THROW(geometry->get_axe("2theta"));
  CPPUNIT_ASSERT_NO_THROW(geometry->get_axe("omega"));
}


void
Diffractometer_TwoC_Test::CrystalPart(void)
{
  CPPUNIT_ASSERT_THROW((*_diffractometer->samples())["toto"],HKLException);
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->addNewCrystal("crystal1"));
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->setCurrentCrystal("crystal1"));
  CPPUNIT_ASSERT_THROW(_diffractometer->addNewCrystal("crystal1"), HKLException);

  CPPUNIT_ASSERT_THROW(_diffractometer->copyCrystalAsNew("toto", "crystal2"), HKLException);
  CPPUNIT_ASSERT_THROW(_diffractometer->copyCrystalAsNew("crystal1", "crystal1"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->copyCrystalAsNew("crystal1", "crystal2"));
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->setCurrentCrystal("crystal2"));


  CPPUNIT_ASSERT_THROW(_diffractometer->delCrystal("toto"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delCrystal("crystal1"));
}

void
Diffractometer_TwoC_Test::renameCrystal(void)
{
  // The currentCrystal is the default crystal.
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->renameCrystal(DEFAULT_CRYSTAL_NAME, "test"));
  // After renaming the currentCrystal must be the new one.
  CPPUNIT_ASSERT_EQUAL(string("test"), _diffractometer->getCurrentCrystalName());

  // The old crystal name must not be found in the crystal list.
  CPPUNIT_ASSERT_THROW(_diffractometer->setCurrentCrystal(DEFAULT_CRYSTAL_NAME), HKLException);
}

void
Diffractometer_TwoC_Test::delCrystal(void)
{
  // When we delete the currentCrystal
  // The currentCrystal must be unset if there is more than one crystal
  // in the crystallist.
  _diffractometer->addNewCrystal("test1");
  _diffractometer->addNewCrystal("test2");
  _diffractometer->setCurrentCrystal("test1");
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delCrystal("test1"));
  CPPUNIT_ASSERT_THROW(_diffractometer->getCurrentCrystalName(), HKLException);

  // When the deleted crystal is not the currentCrystal, the currentCrystal must
  // not be unset.
  _diffractometer->addNewCrystal("test1");
  _diffractometer->setCurrentCrystal("test2");
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delCrystal("test1"));
  CPPUNIT_ASSERT_EQUAL(string("test2"), _diffractometer->getCurrentCrystalName());

  // When we remove the last crystal, the new currentCrystal must be the default one.
  _diffractometer->delCrystal(DEFAULT_CRYSTAL_NAME);
  _diffractometer->delCrystal("test2");
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), _diffractometer->getCurrentCrystalName());
}

void
Diffractometer_TwoC_Test::delAllCrystals(void)
{
  // Add another crystal than the default one.
  _diffractometer->addNewCrystal("test");
  _diffractometer->setCurrentCrystal("test");
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delAllCrystals());
  // The new currentCrystal must be the first crystal in the crystal List
  // here the default one.
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), _diffractometer->getCurrentCrystalName());
}

void
Diffractometer_TwoC_Test::GetSetLattice(void)
{
  // je suis obligé de les initialiser sinon valgrind proteste lors
  // de l'appel du premier setLattice
  double a = 0;
  double b = 0;
  double c = 0;
  double alpha = 0;
  double beta = 0;
  double gamma = 0;

  _diffractometer->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(_diffractometer->getCrystalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
  CPPUNIT_ASSERT_THROW(_diffractometer->setCrystalLattice("toto", a, b, c, alpha, beta, gamma), HKLException);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->setCrystalLattice("crystal", a, b, c, alpha, beta, gamma));

  _diffractometer->setCrystalLattice("crystal", 1., 2., 3., 2., 1., 2.);
  _diffractometer->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., b, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(3., c, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., alpha, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., beta, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., gamma, hkl::constant::math::epsilon_0);
}

void
Diffractometer_TwoC_Test::getCrystalParametersNames(void)
{
  _diffractometer->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(_diffractometer->getCrystalParametersNames("toto"), HKLException);

  vector<string> names;
  CPPUNIT_ASSERT_NO_THROW(names = _diffractometer->getCrystalParametersNames("crystal"));
  CPPUNIT_ASSERT_EQUAL(string("a"), names[0]);
  CPPUNIT_ASSERT_EQUAL(string("b"), names[1]);
  CPPUNIT_ASSERT_EQUAL(string("c"), names[2]);
  CPPUNIT_ASSERT_EQUAL(string("alpha"), names[3]);
  CPPUNIT_ASSERT_EQUAL(string("beta"), names[4]);
  CPPUNIT_ASSERT_EQUAL(string("gamma"), names[5]);
  CPPUNIT_ASSERT_EQUAL(string("euler_x"), names[6]);
  CPPUNIT_ASSERT_EQUAL(string("euler_y"), names[7]);
  CPPUNIT_ASSERT_EQUAL(string("euler_z"), names[8]);
}

void
Diffractometer_TwoC_Test::GetReciprocalLattice(void)
{
  double a, b, c, alpha, beta, gamma;

  _diffractometer->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(_diffractometer->getCrystalReciprocalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));

  _diffractometer->setCrystalLattice("crystal", 1., 2., 3., 90. * hkl::constant::math::degToRad, 90. * hkl::constant::math::degToRad, 90. * hkl::constant::math::degToRad);
  _diffractometer->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau, a, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau / 2., b, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau / 3., c, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, alpha, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, beta, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, gamma, hkl::constant::math::epsilon_0);
}

void
Diffractometer_TwoC_Test::AddReflection(void)
{
  _diffractometer->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(_diffractometer->addCrystalReflection("toto", 0, 0, 1, Best, true), HKLException);

  //even if the crystal exist, the wavelength must be set.
  CPPUNIT_ASSERT_THROW(_diffractometer->addCrystalReflection("crystal",
                       0, 0, 1,
                       Best, true),
                       HKLException);

  _diffractometer->setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true));
}

void
Diffractometer_TwoC_Test::DelReflection(void)
{
  _diffractometer->setWaveLength(1.54);
  _diffractometer->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(_diffractometer->delCrystalReflection("toto", 0), HKLException);

  _diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true);
  _diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delCrystalReflection("crystal", 1));
  CPPUNIT_ASSERT_THROW(_diffractometer->delCrystalReflection("crystal", 1), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->delCrystalReflection("crystal", 0));
  CPPUNIT_ASSERT_THROW(_diffractometer->delCrystalReflection("crystal", 0), HKLException);
}

   void
   diffractometerTest::GetReflection()
   {
   Diffractometer *d = new Diffractometer_Eulerian4C();

   _diffractometer->addNewCrystal("crystal");
   _diffractometer->setCrystal("crystal");

   CPPUNIT_ASSERT_THROW(_diffractometer->getReflection(0), HKLException);
   CPPUNIT_ASSERT_THROW(_diffractometer->getCrystalReflection("toto", 0), HKLException);

   _diffractometer->addReflection(0, 0, 1, Reflection::Best, true);

   CPPUNIT_ASSERT_NO_THROW(_diffractometer->getReflection(0));
   CPPUNIT_ASSERT_NO_THROW(_diffractometer->getCrystalReflection("crystal", 0));

   delete d;
   }


void
Diffractometer_TwoC_Test::ModePart(void)
{
  // Test each mode.
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->setCurrentMode("Symetric"));
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->setCurrentMode("Fix incidence"));

  // try to set an unknown mode ans check if the currentMode is the last valid currentMode.
  CPPUNIT_ASSERT_THROW(_diffractometer->setCurrentMode("toto"), HKLException);
  CPPUNIT_ASSERT_EQUAL(string("Fix incidence"), _diffractometer->getCurrentModeName());

  // test the parameters
  CPPUNIT_ASSERT_THROW(_diffractometer->setModeParameterValue("Symetric", "titi", 10.), HKLException);
  CPPUNIT_ASSERT_THROW(_diffractometer->setModeParameterValue("Fix incidence", "titi", 10.), HKLException);
}

   void
   diffractometerTest::ComputeU()
   {
   Diffractometer *d = new Diffractometer_Eulerian4C();
   _diffractometer->setWaveLength(1.54);
//_diffractometer->setIncidentBeamDirection(1., 0., 0.);

_diffractometer->addNewCrystal("crystal1");
_diffractometer->setCurrentCrystal("crystal1");

CPPUNIT_ASSERT_THROW(_diffractometer->computeU(), HKLException);
_diffractometer->setCrystalLattice("crystal1",
1.54, 1.54, 1.54,
90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );


CPPUNIT_ASSERT_THROW(_diffractometer->computeU(), HKLException);

_diffractometer->setAxeValue("2theta", 60.*hkl::constant::math::degToRad);
_diffractometer->setAxeValue("omega", 30.*hkl::constant::math::degToRad);
_diffractometer->setAxeValue("chi", 0.);
_diffractometer->setAxeValue("phi", 90.*hkl::constant::math::degToRad);
_diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);

CPPUNIT_ASSERT_THROW(_diffractometer->computeU(), HKLException);

_diffractometer->setAxeValue("phi", 180.*hkl::constant::math::degToRad);
_diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);

CPPUNIT_ASSERT_NO_THROW(_diffractometer->computeU());

smatrix M(1., 0., 0.,
0., 0., 1.,
0., -1., 0.);

CPPUNIT_ASSERT_EQUAL(M, _diffractometer->getCurrentCrystal().get_U());

delete d;
}

void
Diffractometer_TwoC_Test::ComputeHKL(void)
{
  double h, k, l;

  _diffractometer->setWaveLength(1.54);

  _diffractometer->addNewCrystal("crystal1");
  _diffractometer->setCurrentCrystal("crystal1");
  _diffractometer->setCrystalLattice("crystal1", 1.54, 1.54, 1.54,
                                      90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );

  _diffractometer->setAxeValue("2theta", 60.*hkl::constant::math::degToRad);
  _diffractometer->setAxeValue("omega", 30.*hkl::constant::math::degToRad);
  _diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Best, true);

  _diffractometer->setAxeValue("omega", 120.*hkl::constant::math::degToRad);
  _diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Best, true);
  _diffractometer->computeU();

  _diffractometer->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);

  _diffractometer->setAxeValue("omega", 30.*hkl::constant::math::degToRad);
  _diffractometer->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);

  _diffractometer->setAxeValue("2theta", 180.*hkl::constant::math::degToRad);
  _diffractometer->setAxeValue("omega", 90.*hkl::constant::math::degToRad);
  _diffractometer->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);
}

void
Diffractometer_TwoC_Test::ComputeAngles(void)
{
  CPPUNIT_ASSERT_THROW(_diffractometer->computeAngles(1., 1., 1.), HKLException);

  _diffractometer->setCurrentMode("Symetric");
  CPPUNIT_ASSERT_THROW(_diffractometer->computeAngles(1., 1., 1.), HKLException);

  _diffractometer->setWaveLength(1.);
  CPPUNIT_ASSERT_THROW(_diffractometer->computeAngles(1., 1., 1.), HKLException);
  CPPUNIT_ASSERT_THROW(_diffractometer->computeAngles(0., 0., 0.), HKLException);

  _diffractometer->addNewCrystal("crystal1");
  _diffractometer->setCurrentCrystal("crystal1");
  CPPUNIT_ASSERT_THROW(_diffractometer->computeAngles(1., 1., 1.), HKLException);

  _diffractometer->setCrystalLattice("crystal1", 1., 1., 1.,
                                      90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );

  _diffractometer->setAxeValue("2theta", 60.*hkl::constant::math::degToRad);
  _diffractometer->setAxeValue("omega", 30.*hkl::constant::math::degToRad);
  _diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Best, true);

  _diffractometer->setAxeValue("omega", 120.*hkl::constant::math::degToRad);
  _diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Best, true);
  _diffractometer->computeU();

  //Symetric
  CPPUNIT_ASSERT_NO_THROW(_diffractometer->computeAngles(1., 0., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*hkl::constant::math::degToRad, _diffractometer->getAxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*hkl::constant::math::degToRad, _diffractometer->getAxeValue("omega"), hkl::constant::math::epsilon_0);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->computeAngles(0., 1., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*hkl::constant::math::degToRad, _diffractometer->getAxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*hkl::constant::math::degToRad, _diffractometer->getAxeValue("omega"), hkl::constant::math::epsilon_0);

  //Fix incidence
  _diffractometer->setCurrentMode("Fix incidence");
  _diffractometer->setAxeValue("omega", 120.*hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->computeAngles(1., 0., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*hkl::constant::math::degToRad, _diffractometer->getAxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(120*hkl::constant::math::degToRad, _diffractometer->getAxeValue("omega"), hkl::constant::math::epsilon_0);

  CPPUNIT_ASSERT_NO_THROW(_diffractometer->computeAngles(0., 2., 0.));
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180*hkl::constant::math::degToRad, _diffractometer->getAxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(120*hkl::constant::math::degToRad, _diffractometer->getAxeValue("omega"), hkl::constant::math::epsilon_0);
}
*/

void
Diffractometer_TwoC_Test::persistanceIO(void)
{
  hkl::twoC::vertical::Diffractometer d_ref;
  hkl::twoC::vertical::Diffractometer d;
  std::stringstream flux;

  d_ref.geometry()->get_source().setWaveLength(2.43);
  d_ref.samples().add("titi", hkl::SAMPLE_MONOCRYSTAL);
  d_ref.samples().set_current("titi");
  hkl::Lattice & lattice = d_ref.samples().current()->lattice();
  lattice.a().set_current(2.34);
  lattice.b().set_current(4.5);
  lattice.c().set_current(2.7);
  lattice.alpha().set_current(90 * hkl::constant::math::degToRad);
  lattice.beta().set_current(120 * hkl::constant::math::degToRad);
  lattice.gamma().set_current(60 * hkl::constant::math::degToRad);
  //d_ref.modes().set_current("Symetric");

  //Add reflections.
  hkl::Axe * tth = d_ref.geometry()->get_axe("tth");
  hkl::Axe * omega = d_ref.geometry()->get_axe("omega");

  // Reflection 1
  tth->set_current(30.398*hkl::constant::math::degToRad);
  omega->set_current(11.709*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 0., 1.));

  // Reflection 2
  tth->set_current(21.001*hkl::constant::math::degToRad);
  omega->set_current(10.322*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 2., 0.));

  // Reflection 3
  tth->set_current(54.046*hkl::constant::math::degToRad);
  omega->set_current(26.872*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-2., 2., 1.));

  // Reflection 4
  tth->set_current(37.333*hkl::constant::math::degToRad);
  omega->set_current(18.51*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-1., 1., 1.));

  d_ref.toStream(flux);
  d.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
}
