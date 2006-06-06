#include "diffractometer_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerTest );

void
DiffractometerTest::setUp(void)
{}

void 
DiffractometerTest::tearDown(void) 
{}

void
DiffractometerTest::GetSetAxe(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();

  
  CPPUNIT_ASSERT_THROW(d->setAxeValue("nu", 0.), HKLException);
  CPPUNIT_ASSERT_THROW(d->getAxeValue("nu"), HKLException);
  
  CPPUNIT_ASSERT_NO_THROW(d->getAxeValue("2theta"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeValue("omega"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeValue("chi"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeValue("phi"));

  CPPUNIT_ASSERT_NO_THROW(d->setAxeValue("2theta", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeValue("omega", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeValue("chi", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeValue("phi", 1.));
  
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeValue("2theta"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeValue("omega"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeValue("chi"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeValue("phi"));

  delete d;
}


void
DiffractometerTest::CrystalPart(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  CPPUNIT_ASSERT_THROW(d->setCurrentCrystal("toto"),HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->addNewCrystal("crystal1"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentCrystal("crystal1"));
  CPPUNIT_ASSERT_THROW(d->addNewCrystal("crystal1"), HKLException);
  
  CPPUNIT_ASSERT_THROW(d->copyCrystalAsNew("toto", "crystal2"), HKLException);
  CPPUNIT_ASSERT_THROW(d->copyCrystalAsNew("crystal1", "crystal1"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->copyCrystalAsNew("crystal1", "crystal2"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentCrystal("crystal2"));
  
  
  CPPUNIT_ASSERT_THROW(d->delCrystal("toto"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->delCrystal("crystal1"));
  
  delete d;
}

void
DiffractometerTest::renameCrystal(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  // The currentCrystal is the default crystal.
  CPPUNIT_ASSERT_NO_THROW(d->renameCrystal(DEFAULT_CRYSTAL_NAME, "test"));
  // After renaming the currentCrystal must be the new one.
  CPPUNIT_ASSERT_EQUAL(string("test"), d->getCurrentCrystalName());

  // The old crystal name must not be found in the crystal list.
  CPPUNIT_ASSERT_THROW(d->setCurrentCrystal(DEFAULT_CRYSTAL_NAME), HKLException);
  
  delete d;
}

void
DiffractometerTest::delCrystal(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  // When we delete the currentCrystal
  // The currentCrystal must be unset if there is more than one crystal
  // in the crystallist.
  d->addNewCrystal("test1");
  d->addNewCrystal("test2");
  d->setCurrentCrystal("test1");
  CPPUNIT_ASSERT_NO_THROW(d->delCrystal("test1"));
  CPPUNIT_ASSERT_THROW(d->getCurrentCrystalName(), HKLException);

  // When the deleted crystal is not the currentCrystal, the currentCrystal must
  // not be unset.  
  d->addNewCrystal("test1");
  d->setCurrentCrystal("test2");
  CPPUNIT_ASSERT_NO_THROW(d->delCrystal("test1"));
  CPPUNIT_ASSERT_EQUAL(string("test2"), d->getCurrentCrystalName());
  
  // When we remove the last crystal, the new currentCrystal must be the default one.
  d->delCrystal(DEFAULT_CRYSTAL_NAME);
  d->delCrystal("test2");
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), d->getCurrentCrystalName());
  
  delete d;
}

void
DiffractometerTest::delAllCrystals(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  // Add another crystal than the default one.
  d->addNewCrystal("test");
  d->setCurrentCrystal("test");
  CPPUNIT_ASSERT_NO_THROW(d->delAllCrystals());
  // The new currentCrystal must be the first crystal in the crystal List
  // here the default one.
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), d->getCurrentCrystalName());

  delete d;
}

void
DiffractometerTest::GetSetLattice(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  // je suis obligé de les initialiser sinon valgrind proteste lors 
  // de l'appel du premier setLattice
  double a = 0;
  double b = 0;
  double c = 0;
  double alpha = 0;
  double beta = 0;
  double gamma = 0;
  
  d->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(d->getCrystalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
  CPPUNIT_ASSERT_THROW(d->setCrystalLattice("toto", a, b, c, alpha, beta, gamma), HKLException);
  
  CPPUNIT_ASSERT_NO_THROW(d->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));
  CPPUNIT_ASSERT_NO_THROW(d->setCrystalLattice("crystal", a, b, c, alpha, beta, gamma));

  d->setCrystalLattice("crystal", 1., 2., 3., 2., 1., 2.);
  d->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(3., c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., gamma, constant::math::epsilon_0);
  
  delete d;
}

void
DiffractometerTest::getCrystalParametersNames(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  d->addNewCrystal("crystal");
  
  CPPUNIT_ASSERT_THROW(d->getCrystalParametersNames("toto"), HKLException);
  
  vector<string> names;
  CPPUNIT_ASSERT_NO_THROW(names = d->getCrystalParametersNames("crystal"));
  CPPUNIT_ASSERT_EQUAL(string("a"), names[0]);
  CPPUNIT_ASSERT_EQUAL(string("b"), names[1]);
  CPPUNIT_ASSERT_EQUAL(string("c"), names[2]);
  CPPUNIT_ASSERT_EQUAL(string("alpha"), names[3]);
  CPPUNIT_ASSERT_EQUAL(string("beta"), names[4]);
  CPPUNIT_ASSERT_EQUAL(string("gamma"), names[5]);
  CPPUNIT_ASSERT_EQUAL(string("euler_x"), names[6]);
  CPPUNIT_ASSERT_EQUAL(string("euler_y"), names[7]);
  CPPUNIT_ASSERT_EQUAL(string("euler_z"), names[8]);  
  delete d;
}

void
DiffractometerTest::GetReciprocalLattice(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  double a, b, c, alpha, beta, gamma;
  
  d->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(d->getCrystalReciprocalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
  
  CPPUNIT_ASSERT_NO_THROW(d->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));

  d->setCrystalLattice("crystal", 1., 2., 3., 90. * constant::math::degToRad, 90. * constant::math::degToRad, 90. * constant::math::degToRad);
  d->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau, a, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 2., b, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 3., c, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, gamma, constant::math::epsilon_0);
    
  delete d;
}

void
DiffractometerTest::AddReflection(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  d->addNewCrystal("crystal");
  
  CPPUNIT_ASSERT_THROW(d->addCrystalReflection("toto", 0, 0, 1, Reflection::Best, true), HKLException);
  
  //even if the crystal exist, the wavelength must be set.
  CPPUNIT_ASSERT_THROW(d->addCrystalReflection("crystal",
                                               0, 0, 1,
                                               Reflection::Best, true),
                       HKLException);
  
  d->setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(d->addCrystalReflection("crystal", 0, 0, 1, Reflection::Best, true));
  delete d;
}

void
DiffractometerTest::DelReflection(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  d->setWaveLength(1.54);
  d->addNewCrystal("crystal");
  
  CPPUNIT_ASSERT_THROW(d->delCrystalReflection("toto", 0), HKLException);
  
  d->addCrystalReflection("crystal", 0, 0, 1, Reflection::Best, true);
  d->addCrystalReflection("crystal", 0, 0, 1, Reflection::Best, true);
  
  CPPUNIT_ASSERT_NO_THROW(d->delCrystalReflection("crystal", 1));
  CPPUNIT_ASSERT_THROW(d->delCrystalReflection("crystal", 1), HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->delCrystalReflection("crystal", 0));
  CPPUNIT_ASSERT_THROW(d->delCrystalReflection("crystal", 0), HKLException);
  
  delete d;
}

/*
void
diffractometerTest::GetReflection()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();
  
  d->addNewCrystal("crystal");
  d->setCrystal("crystal");
  
  CPPUNIT_ASSERT_THROW(d->getReflection(0), HKLException);
  CPPUNIT_ASSERT_THROW(d->getCrystalReflection("toto", 0), HKLException);
   
  d->addReflection(0, 0, 1, Reflection::Best, true); 
  
  CPPUNIT_ASSERT_NO_THROW(d->getReflection(0));
  CPPUNIT_ASSERT_NO_THROW(d->getCrystalReflection("crystal", 0));

  delete d;
}
*/


void
DiffractometerTest::ModePart(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
 
  // Test each mode.
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Bissector"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Delta Theta"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Constant Omega"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Constant Chi"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Constant Phi"));

  // try to set an unknown mode ans check if the currentMode is the last valid currentMode.
  CPPUNIT_ASSERT_THROW(d->setCurrentMode("toto"), HKLException);
  CPPUNIT_ASSERT_EQUAL(string("Constant Phi"), d->getCurrentModeName());
 
  // test the parameters
  CPPUNIT_ASSERT_THROW(d->setModeParameterValue("Bissector", "titi", 10.), HKLException);
  
  CPPUNIT_ASSERT_THROW(d->getModeParameterValue("Constant Omega", "titi"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->setModeParameterValue("Constant Omega", "omega", 5.));
  CPPUNIT_ASSERT_NO_THROW(d->getModeParameterValue("Constant Omega", "omega"));
  CPPUNIT_ASSERT_EQUAL(5., d->getModeParameterValue("Constant Omega", "omega"));
  
  delete d;
}

/*
void 
diffractometerTest::ComputeU()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();  
  d->setWaveLength(1.54);
  //d->setIncidentBeamDirection(1., 0., 0.);
  
  d->addNewCrystal("crystal1");
  d->setCurrentCrystal("crystal1");
  
  CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
  d->setCrystalLattice("crystal1",
                       1.54, 1.54, 1.54,
                       90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );

  
  CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
  
  d->setAxeValue("2theta", 60.*constant::math::degToRad);  
  d->setAxeValue("omega", 30.*constant::math::degToRad);
  d->setAxeValue("chi", 0.);
  d->setAxeValue("phi", 90.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);
  
  CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
  
  d->setAxeValue("phi", 180.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
  
  CPPUNIT_ASSERT_NO_THROW(d->computeU());
  
  smatrix M(1., 0., 0.,
            0., 0., 1.,
            0., -1., 0.);
            
  CPPUNIT_ASSERT_EQUAL(M, d->getCurrentCrystal().get_U());
  
  delete d;
}
*/


void 
DiffractometerTest::ComputeHKL(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  double h, k, l;
  
  d->setWaveLength(1.54);
  //d->setIncidentBeamDirection(1., 0., 0.);
  
  d->addNewCrystal("crystal1");
  d->setCurrentCrystal("crystal1");
  d->setCrystalLattice("crystal1", 1.54, 1.54, 1.54,
                       90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );
  
  d->setAxeValue("2theta", 60.*constant::math::degToRad);  
  d->setAxeValue("omega", 30.*constant::math::degToRad);
  d->setAxeValue("chi", 0.);
  d->setAxeValue("phi", 90.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);
  
  d->setAxeValue("phi", 180.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  d->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., h, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., k, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);
  
  d->setAxeValue("phi", 90.*constant::math::degToRad);
  d->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., h, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);

  d->setAxeValue("2theta", 180.*constant::math::degToRad);
  d->setAxeValue("omega", 90.*constant::math::degToRad);
  d->computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., h, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);  

  delete d;
}

void 
DiffractometerTest::ComputeAngles(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  CPPUNIT_ASSERT_THROW(d->computeAngles(1., 1., 1.), HKLException);
  
  d->setCurrentMode("Bissector");
  CPPUNIT_ASSERT_THROW(d->computeAngles(1., 1., 1.), HKLException);
  
  d->setWaveLength(1.);
  //d->setIncidentBeamDirection(1., 0., 0.);
  CPPUNIT_ASSERT_THROW(d->computeAngles(1., 1., 1.), HKLException);
  CPPUNIT_ASSERT_THROW(d->computeAngles(0., 0., 0.), HKLException);
  
  d->addNewCrystal("crystal1");
  d->setCurrentCrystal("crystal1");
  CPPUNIT_ASSERT_THROW(d->computeAngles(1., 1., 1.), HKLException);
  
  d->setCrystalLattice("crystal1", 1., 1., 1.,
                       90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );
  
  d->setAxeValue("2theta", 60.*constant::math::degToRad);  
  d->setAxeValue("omega", 30.*constant::math::degToRad);
  d->setAxeValue("chi", 0.);
  d->setAxeValue("phi", 90.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);
  
  d->setAxeValue("phi", 180.*constant::math::degToRad);
  d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  CPPUNIT_ASSERT_NO_THROW(d->computeAngles(1., 0., 0.));
  
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, d->getAxeValue("2theta"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, d->getAxeValue("omega"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, d->getAxeValue("chi"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, d->getAxeValue("phi"), constant::math::epsilon_0);  
  
  delete d;
}

void 
DiffractometerTest::LPS(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  d->setCurrentMode("Bissector");
  
  d->setWaveLength(1.542);
  //d->setIncidentBeamDirection(1., 0., 0.);
  
  d->addNewCrystal("orthorombique");
  d->setCurrentCrystal("orthorombique");
  
  d->setCrystalLattice("orthorombique",
                       4.81, 8.47, 2.941,
                       90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );
  
  d->setAxeValue("2theta", 30.391991*constant::math::degToRad);  
  d->setAxeValue("omega", 15.195995*constant::math::degToRad);
  d->setAxeValue("chi", 90.*constant::math::degToRad);
  d->setAxeValue("phi", 0.*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", 0., 0., 1., Reflection::Best, true);
   
  d->setAxeValue("2theta", 10.445403*constant::math::degToRad);  
  d->setAxeValue("omega", 5.2227013*constant::math::degToRad); 
  d->setAxeValue("chi", 0.*constant::math::degToRad);
  d->setAxeValue("phi", 0.*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  CPPUNIT_ASSERT_NO_THROW(d->computeAngles(0., 1., 0.));
  
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10.445403*constant::math::degToRad, d->getAxeValue("2theta"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(5.2227013*constant::math::degToRad, d->getAxeValue("omega"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, d->getAxeValue("chi"), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, d->getAxeValue("phi"), constant::math::epsilon_0);   
  delete d;
}

void 
DiffractometerTest::LPS2(void)
{
  Diffractometer *d = new diffractometer::Eulerian4C();
  
  d->setCurrentMode("Bissector");
  
  d->setWaveLength(1.5418);
  //d->setIncidentBeamDirection(1., 0., 0.);
  
  d->addNewCrystal("orthorombique");
  d->setCurrentCrystal("orthorombique");
  
  d->setCrystalLattice("orthorombique",
                       4.81, 8.47, 2.941,
                       90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );
  
  // Reflection 1
  d->setAxeValue("2theta", 30.398*constant::math::degToRad);
  d->setAxeValue("omega", 11.709*constant::math::degToRad);
  d->setAxeValue("chi", 87.607*constant::math::degToRad);
  d->setAxeValue("phi", 0.265*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", 0., 0., 1., Reflection::Best, true);
  
  // Reflection 2
  d->setAxeValue("2theta", 21.001*constant::math::degToRad);
  d->setAxeValue("omega", 10.322*constant::math::degToRad);
  d->setAxeValue("chi", -2.139*constant::math::degToRad);
  d->setAxeValue("phi", 0.023*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", 0., 2., 0., Reflection::Best, true);

  // Reflection 3
  d->setAxeValue("2theta", 54.046*constant::math::degToRad);
  d->setAxeValue("omega", 26.872*constant::math::degToRad);
  d->setAxeValue("chi", 34.938*constant::math::degToRad);
  d->setAxeValue("phi", 57.295*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", -2., 2., 1., Reflection::Best, true);

  // Reflection 4
  d->setAxeValue("2theta", 37.333*constant::math::degToRad);
  d->setAxeValue("omega", 18.51*constant::math::degToRad);
  d->setAxeValue("chi", 53.966*constant::math::degToRad);
  d->setAxeValue("phi", 54.505*constant::math::degToRad);
  d->addCrystalReflection("orthorombique", -1., 1., 1., Reflection::Best, true);
 
  d->affineCrystal("orthorombique", "simplex");
  double h, k, l;
  d->computeAngles(0., 0., 1.);
  d->computeHKL(h, k, l);
  
  delete d;
}

void
DiffractometerTest::persistanceIO(void)
{
  diffractometer::Eulerian4C d_ref;
  diffractometer::Eulerian4C d;  
  diffractometer::Eulerian4C d1_ref;
  diffractometer::Eulerian4C d1;
  stringstream flux;
  
  d_ref.setWaveLength(2.43);
  d_ref.addNewCrystal("titi");
  d_ref.setCrystalLattice("titi",
                           2.34, 4.5, 2.7,
                           90*constant::math::degToRad, 120*constant::math::degToRad, 60*constant::math::degToRad);
  d_ref.setCurrentMode("Bissector");
  d_ref.setCurrentCrystal("titi");
 
 // Reflection 1
  d_ref.setAxeValue("2theta", 30.398*constant::math::degToRad);
  d_ref.setAxeValue("omega", 11.709*constant::math::degToRad);
  d_ref.setAxeValue("chi", 87.607*constant::math::degToRad);
  d_ref.setAxeValue("phi", 0.265*constant::math::degToRad);
  d_ref.addCrystalReflection("titi", 0., 0., 1., Reflection::Best, true);
  
  // Reflection 2
  d_ref.setAxeValue("2theta", 21.001*constant::math::degToRad);
  d_ref.setAxeValue("omega", 10.322*constant::math::degToRad);
  d_ref.setAxeValue("chi", -2.139*constant::math::degToRad);
  d_ref.setAxeValue("phi", 0.023*constant::math::degToRad);
  d_ref.addCrystalReflection("titi", 0., 2., 0., Reflection::Best, true);

  // Reflection 3
  d_ref.setAxeValue("2theta", 54.046*constant::math::degToRad);
  d_ref.setAxeValue("omega", 26.872*constant::math::degToRad);
  d_ref.setAxeValue("chi", 34.938*constant::math::degToRad);
  d_ref.setAxeValue("phi", 57.295*constant::math::degToRad);
  d_ref.addCrystalReflection("titi", -2., 2., 1., Reflection::Best, true);

  // Reflection 4
  d_ref.setAxeValue("2theta", 37.333*constant::math::degToRad);
  d_ref.setAxeValue("omega", 18.51*constant::math::degToRad);
  d_ref.setAxeValue("chi", 53.966*constant::math::degToRad);
  d_ref.setAxeValue("phi", 54.505*constant::math::degToRad);
  d_ref.addCrystalReflection("titi", -1., 1., 1., Reflection::Best, true);
  
  d_ref.toStream(flux);
  d1_ref.toStream(flux);
  d.fromStream(flux);
  d1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
  CPPUNIT_ASSERT_EQUAL(d1_ref, d1);
}
