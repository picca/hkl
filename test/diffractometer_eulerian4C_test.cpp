#include "diffractometer_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( DiffractometerEulerian4CTest );

void
DiffractometerEulerian4CTest::setUp(void)
{}

void
DiffractometerEulerian4CTest::tearDown(void)
{}

/*
void
DiffractometerEulerian4CTest::GetSethkl::Axe(void)
{
  CPPUNIT_ASSERT_THROW(m_d.sethkl::AxeValue("nu", 0.), HKLException);
  CPPUNIT_ASSERT_THROW(m_d.gethkl::AxeValue("nu"), HKLException);
 
  CPPUNIT_ASSERT_NO_THROW(m_d.gethkl::AxeValue("omega"));
  CPPUNIT_ASSERT_NO_THROW(m_d.gethkl::AxeValue("chi"));
  CPPUNIT_ASSERT_NO_THROW(m_d.gethkl::AxeValue("phi"));
  CPPUNIT_ASSERT_NO_THROW(m_d.gethkl::AxeValue("2theta"));
 
  CPPUNIT_ASSERT_NO_THROW(m_d.sethkl::AxeValue("omega", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_d.sethkl::AxeValue("chi", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_d.sethkl::AxeValue("phi", 1.));
  CPPUNIT_ASSERT_NO_THROW(m_d.sethkl::AxeValue("2theta", 1.));
 
  CPPUNIT_ASSERT_EQUAL(1., m_d.gethkl::AxeValue("omega"));
  CPPUNIT_ASSERT_EQUAL(1., m_d.gethkl::AxeValue("chi"));
  CPPUNIT_ASSERT_EQUAL(1., m_d.gethkl::AxeValue("phi"));
  CPPUNIT_ASSERT_EQUAL(1., m_d.gethkl::AxeValue("2theta"));
}
 
void
DiffractometerEulerian4CTest::CrystalPart(void)
{
  CPPUNIT_ASSERT_THROW(m_d.setCurrentCrystal("toto"),HKLException);
  CPPUNIT_ASSERT_NO_THROW(m_d.addNewCrystal("crystal1"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentCrystal("crystal1"));
  CPPUNIT_ASSERT_THROW(m_d.addNewCrystal("crystal1"), HKLException);
 
  CPPUNIT_ASSERT_THROW(m_d.copyCrystalAsNew("toto", "crystal2"), HKLException);
  CPPUNIT_ASSERT_THROW(m_d.copyCrystalAsNew("crystal1", "crystal1"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(m_d.copyCrystalAsNew("crystal1", "crystal2"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentCrystal("crystal2"));
 
 
  CPPUNIT_ASSERT_THROW(m_d.delCrystal("toto"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(m_d.delCrystal("crystal1"));
}
 
void
DiffractometerEulerian4CTest::renameCrystal(void)
{
  // The currentCrystal is the default crystal.
  CPPUNIT_ASSERT_NO_THROW(m_d.renameCrystal(DEFAULT_CRYSTAL_NAME, "test"));
  // After renaming the currentCrystal must be the new one.
  CPPUNIT_ASSERT_EQUAL(string("test"), m_d.getCurrentCrystalName());
 
  // The old crystal name must not be found in the crystal list.
  CPPUNIT_ASSERT_THROW(m_d.setCurrentCrystal(DEFAULT_CRYSTAL_NAME), HKLException);
}
 
void
DiffractometerEulerian4CTest::delCrystal(void)
{
  // When we delete the currentCrystal
  // The currentCrystal must be unset if there is more than one crystal
  // in the crystallist.
  m_d.addNewCrystal("test1");
  m_d.addNewCrystal("test2");
  m_d.setCurrentCrystal("test1");
  CPPUNIT_ASSERT_NO_THROW(m_d.delCrystal("test1"));
  CPPUNIT_ASSERT_THROW(m_d.getCurrentCrystalName(), HKLException);
 
  // When the deleted crystal is not the currentCrystal, the currentCrystal must
  // not be unset.
  m_d.addNewCrystal("test1");
  m_d.setCurrentCrystal("test2");
  CPPUNIT_ASSERT_NO_THROW(m_d.delCrystal("test1"));
  CPPUNIT_ASSERT_EQUAL(string("test2"), m_d.getCurrentCrystalName());
 
  // When we remove the last crystal, the new currentCrystal must be the default one.
  m_d.delCrystal(DEFAULT_CRYSTAL_NAME);
  m_d.delCrystal("test2");
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), m_d.getCurrentCrystalName());
}
 
void
DiffractometerEulerian4CTest::delAllCrystals(void)
{
  // Add another crystal than the default one.
  m_d.addNewCrystal("test");
  m_d.setCurrentCrystal("test");
  CPPUNIT_ASSERT_NO_THROW(m_d.delAllCrystals());
  // The new currentCrystal must be the first crystal in the crystal List
  // here the default one.
  CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), m_d.getCurrentCrystalName());
}
 
void
DiffractometerEulerian4CTest::GetSetLattice(void)
{
  // je suis obligé de les initialiser sinon valgrind proteste lors
  // de l'appel du premier setLattice
  double a = 0;
  double b = 0;
  double c = 0;
  double alpha = 0;
  double beta = 0;
  double gamma = 0;
 
  m_d.addNewCrystal("crystal");
 
  CPPUNIT_ASSERT_THROW(m_d.getCrystalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
  CPPUNIT_ASSERT_THROW(m_d.setCrystalLattice("toto", a, b, c, alpha, beta, gamma), HKLException);
 
  CPPUNIT_ASSERT_NO_THROW(m_d.getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCrystalLattice("crystal", a, b, c, alpha, beta, gamma));
 
  m_d.setCrystalLattice("crystal", 1., 2., 3., 2., 1., 2.);
  m_d.getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., b, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(3., c, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., alpha, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., beta, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., gamma, hkl::constant::math::epsilon_0);
}
 
void
DiffractometerEulerian4CTest::getCrystalParametersNames(void)
{
  m_d.addNewCrystal("crystal");
 
  CPPUNIT_ASSERT_THROW(m_d.getCrystalParametersNames("toto"), HKLException);
 
  vector<string> names;
  CPPUNIT_ASSERT_NO_THROW(names = m_d.getCrystalParametersNames("crystal"));
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
DiffractometerEulerian4CTest::GetReciprocalLattice(void)
{
  double a, b, c, alpha, beta, gamma;
 
  m_d.addNewCrystal("crystal");
 
  CPPUNIT_ASSERT_THROW(m_d.getCrystalReciprocalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
 
  CPPUNIT_ASSERT_NO_THROW(m_d.getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));
 
  m_d.setCrystalLattice("crystal", 1., 2., 3., 90. * hkl::constant::math::degToRad, 90. * hkl::constant::math::degToRad, 90. * hkl::constant::math::degToRad);
  m_d.getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau, a, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau / 2., b, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(hkl::constant::physic::tau / 3., c, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, alpha, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, beta, hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * hkl::constant::math::degToRad, gamma, hkl::constant::math::epsilon_0);
}
 
void
DiffractometerEulerian4CTest::AddReflection(void)
{
  m_d.addNewCrystal("crystal");
 
  CPPUNIT_ASSERT_THROW(m_d.addCrystalReflection("toto", 0, 0, 1, Best, true), HKLException);
 
  //even if the crystal exist, the wavelength must be set.
  CPPUNIT_ASSERT_THROW(m_d.addCrystalReflection("crystal",
                       0, 0, 1,
                       Best, true),
                       HKLException);
 
  m_d.setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(m_d.addCrystalReflection("crystal", 0, 0, 1, Best, true));
}
 
void
DiffractometerEulerian4CTest::DelReflection(void)
{
  m_d.setWaveLength(1.54);
  m_d.addNewCrystal("crystal");
 
  CPPUNIT_ASSERT_THROW(m_d.delCrystalReflection("toto", 0), HKLException);
 
  m_d.addCrystalReflection("crystal", 0, 0, 1, Best, true);
  m_d.addCrystalReflection("crystal", 0, 0, 1, Best, true);
 
  CPPUNIT_ASSERT_NO_THROW(m_d.delCrystalReflection("crystal", 1));
  CPPUNIT_ASSERT_THROW(m_d.delCrystalReflection("crystal", 1), HKLException);
  CPPUNIT_ASSERT_NO_THROW(m_d.delCrystalReflection("crystal", 0));
  CPPUNIT_ASSERT_THROW(m_d.delCrystalReflection("crystal", 0), HKLException);
}
 
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
 
 
void
DiffractometerEulerian4CTest::ModePart(void)
{
  // Test each mode.
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentMode("Bissector"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentMode("Delta Theta"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentMode("Constant Omega"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentMode("Constant Chi"));
  CPPUNIT_ASSERT_NO_THROW(m_d.setCurrentMode("Constant Phi"));
 
  // try to set an unknown mode ans check if the currentMode is the last valid currentMode.
  CPPUNIT_ASSERT_THROW(m_d.setCurrentMode("toto"), HKLException);
  CPPUNIT_ASSERT_EQUAL(string("Constant Phi"), m_d.getCurrentModeName());
 
  // test the parameters
  CPPUNIT_ASSERT_THROW(m_d.setModeParameterValue("Bissector", "titi", 10.), HKLException);
 
  CPPUNIT_ASSERT_THROW(m_d.getModeParameterValue("Constant Omega", "titi"), HKLException);
  CPPUNIT_ASSERT_NO_THROW(m_d.setModeParameterValue("Constant Omega", "omega", 5.));
  CPPUNIT_ASSERT_NO_THROW(m_d.getModeParameterValue("Constant Omega", "omega"));
  CPPUNIT_ASSERT_EQUAL(5., m_d.getModeParameterValue("Constant Omega", "omega"));
}
 
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
90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );
 
 
CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
 
d->sethkl::AxeValue("2theta", 60.*hkl::constant::math::degToRad);  
d->sethkl::AxeValue("omega", 30.*hkl::constant::math::degToRad);
d->sethkl::AxeValue("chi", 0.);
d->sethkl::AxeValue("phi", 90.*hkl::constant::math::degToRad);
d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);
 
CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
 
d->sethkl::AxeValue("phi", 180.*hkl::constant::math::degToRad);
d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
 
CPPUNIT_ASSERT_NO_THROW(d->computeU());
 
smatrix M(1., 0., 0.,
0., 0., 1.,
0., -1., 0.);
 
CPPUNIT_ASSERT_EQUAL(M, d->getCurrentCrystal().get_U());
 
delete d;
}
 
void
DiffractometerEulerian4CTest::ComputeHKL(void)
{
  double h, k, l;
 
  m_d.setWaveLength(1.54);
  //d->setIncidentBeamDirection(1., 0., 0.);
 
  m_d.addNewCrystal("crystal1");
  m_d.setCurrentCrystal("crystal1");
  m_d.setCrystalLattice("crystal1", 1.54, 1.54, 1.54,
                        90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );
 
  m_d.sethkl::AxeValue("2theta", 60.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 30.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 0.);
  m_d.sethkl::AxeValue("phi", 90.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("crystal1", 1., 0., 0., Best, true);
 
  m_d.sethkl::AxeValue("phi", 180.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("crystal1", 0., 1., 0., Best, true);
  m_d.computeU();
 
  m_d.computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);
 
  m_d.sethkl::AxeValue("phi", 90.*hkl::constant::math::degToRad);
  m_d.computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);
 
  m_d.sethkl::AxeValue("2theta", 180.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 90.*hkl::constant::math::degToRad);
  m_d.computeHKL(h, k, l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., h, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, hkl::constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, hkl::constant::math::epsilon_1);
}
 
void
DiffractometerEulerian4CTest::ComputeAngles(void)
{
  CPPUNIT_ASSERT_THROW(m_d.computeAngles(1., 1., 1.), HKLException);
 
  m_d.setCurrentMode("Bissector");
  CPPUNIT_ASSERT_THROW(m_d.computeAngles(1., 1., 1.), HKLException);
 
  m_d.setWaveLength(1.);
  //d->setIncidentBeamDirection(1., 0., 0.);
  CPPUNIT_ASSERT_THROW(m_d.computeAngles(1., 1., 1.), HKLException);
  CPPUNIT_ASSERT_THROW(m_d.computeAngles(0., 0., 0.), HKLException);
 
  m_d.addNewCrystal("crystal1");
  m_d.setCurrentCrystal("crystal1");
  CPPUNIT_ASSERT_THROW(m_d.computeAngles(1., 1., 1.), HKLException);
 
  m_d.setCrystalLattice("crystal1", 1., 1., 1.,
                        90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );
 
  m_d.sethkl::AxeValue("2theta", 60.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 30.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 0.);
  m_d.sethkl::AxeValue("phi", 90.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("crystal1", 1., 0., 0., Best, true);
 
  m_d.sethkl::AxeValue("phi", 180.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("crystal1", 0., 1., 0., Best, true);
  m_d.computeU();
 
  CPPUNIT_ASSERT_NO_THROW(m_d.computeAngles(1., 0., 0.));
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("omega"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, m_d.gethkl::AxeValue("chi"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("phi"), hkl::constant::math::epsilon_0);
}
 
void
DiffractometerEulerian4CTest::LPS(void)
{
  m_d.setCurrentMode("Bissector");
 
  m_d.setWaveLength(1.542);
  //d->setIncidentBeamDirection(1., 0., 0.);
 
  m_d.addNewCrystal("orthorombique");
  m_d.setCurrentCrystal("orthorombique");
 
  m_d.setCrystalLattice("orthorombique",
                        4.81, 8.47, 2.941,
                        90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );
 
  m_d.sethkl::AxeValue("2theta", 30.391991*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 15.195995*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 90.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 0.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", 0., 0., 1., Best, true);
 
  m_d.sethkl::AxeValue("2theta", 10.445403*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 5.2227013*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 0.*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 0.*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", 0., 1., 0., Best, true);
  m_d.computeU();
 
  CPPUNIT_ASSERT_NO_THROW(m_d.computeAngles(0., 1., 0.));
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(10.445403*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("2theta"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(5.2227013*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("omega"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("chi"), hkl::constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*hkl::constant::math::degToRad, m_d.gethkl::AxeValue("phi"), hkl::constant::math::epsilon_0);
}
 
void
DiffractometerEulerian4CTest::LPS2(void)
{
  m_d.setCurrentMode("Bissector");
 
  m_d.setWaveLength(1.5418);
  //d->setIncidentBeamDirection(1., 0., 0.);
 
  m_d.addNewCrystal("orthorombique");
  m_d.setCurrentCrystal("orthorombique");
 
  m_d.setCrystalLattice("orthorombique",
                        4.81, 8.47, 2.941,
                        90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad, 90.*hkl::constant::math::degToRad );
 
  // Reflection 1
  m_d.sethkl::AxeValue("2theta", 30.398*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 11.709*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 87.607*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 0.265*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", 0., 0., 1., Best, true);
 
  // Reflection 2
  m_d.sethkl::AxeValue("2theta", 21.001*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 10.322*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", -2.139*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 0.023*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", 0., 2., 0., Best, true);
 
  // Reflection 3
  m_d.sethkl::AxeValue("2theta", 54.046*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 26.872*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 34.938*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 57.295*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", -2., 2., 1., Best, true);
 
  // Reflection 4
  m_d.sethkl::AxeValue("2theta", 37.333*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("omega", 18.51*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("chi", 53.966*hkl::constant::math::degToRad);
  m_d.sethkl::AxeValue("phi", 54.505*hkl::constant::math::degToRad);
  m_d.addCrystalReflection("orthorombique", -1., 1., 1., Best, true);
 
  m_d.affineCrystal("orthorombique", "simplex");
  double h, k, l;
  m_d.computeAngles(0., 0., 1.);
  m_d.computeHKL(h, k, l);
}
 
*/

void
DiffractometerEulerian4CTest::persistanceIO(void)
{
  hkl::eulerian4C::vertical::Diffractometer d_ref;
  hkl::eulerian4C::vertical::Diffractometer d;
  stringstream flux;

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
  hkl::Axe * tth = d_ref.geometry()->get_axe("2theta");
  hkl::Axe * omega = d_ref.geometry()->get_axe("omega");
  hkl::Axe * chi = d_ref.geometry()->get_axe("chi");
  hkl::Axe * phi = d_ref.geometry()->get_axe("phi");

  // Reflection 1
  tth->set_current(30.398*hkl::constant::math::degToRad);
  omega->set_current(11.709*hkl::constant::math::degToRad);
  chi->set_current(87.607*hkl::constant::math::degToRad);
  phi->set_current(0.265*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 0., 1.));

  // Reflection 2
  tth->set_current(21.001*hkl::constant::math::degToRad);
  omega->set_current(10.322*hkl::constant::math::degToRad);
  chi->set_current(-2.139*hkl::constant::math::degToRad);
  phi->set_current(0.023*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(0., 2., 0.));

  // Reflection 3
  tth->set_current(54.046*hkl::constant::math::degToRad);
  omega->set_current(26.872*hkl::constant::math::degToRad);
  chi->set_current(34.938*hkl::constant::math::degToRad);
  phi->set_current(57.295*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-2., 2., 1.));

  // Reflection 4
  tth->set_current(37.333*hkl::constant::math::degToRad);
  omega->set_current(18.51*hkl::constant::math::degToRad);
  chi->set_current(53.966*hkl::constant::math::degToRad);
  phi->set_current(54.505*hkl::constant::math::degToRad);
  d_ref.samples().current()->reflections().add(hkl::svector(-1., 1., 1.));

  d_ref.toStream(flux);
  d.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(d_ref, d);
}
