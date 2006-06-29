#include "diffractometer_twoC_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Diffractometer_TwoC_Test );

void
Diffractometer_TwoC_Test::setUp(void)
{
    m_diffractometer = new diffractometer::twoC::Vertical;
}

void 
Diffractometer_TwoC_Test::tearDown(void) 
{
    delete m_diffractometer;
}

void
Diffractometer_TwoC_Test::GetSetAxe(void)
{
    CPPUNIT_ASSERT_THROW(m_diffractometer->setAxeValue("titi", 0.), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->getAxeValue("nu"), HKLException);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("2theta"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getAxeValue("omega"));

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("2theta", 1.));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setAxeValue("omega", 1.));

    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("2theta"));
    CPPUNIT_ASSERT_EQUAL(1., m_diffractometer->getAxeValue("omega"));
}


void
Diffractometer_TwoC_Test::CrystalPart(void)
{
    CPPUNIT_ASSERT_THROW(m_diffractometer->setCurrentCrystal("toto"),HKLException);
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->addNewCrystal("crystal1"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setCurrentCrystal("crystal1"));
    CPPUNIT_ASSERT_THROW(m_diffractometer->addNewCrystal("crystal1"), HKLException);

    CPPUNIT_ASSERT_THROW(m_diffractometer->copyCrystalAsNew("toto", "crystal2"), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->copyCrystalAsNew("crystal1", "crystal1"), HKLException);
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->copyCrystalAsNew("crystal1", "crystal2"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setCurrentCrystal("crystal2"));


    CPPUNIT_ASSERT_THROW(m_diffractometer->delCrystal("toto"), HKLException);
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delCrystal("crystal1"));
}

void
Diffractometer_TwoC_Test::renameCrystal(void)
{
    // The currentCrystal is the default crystal.
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->renameCrystal(DEFAULT_CRYSTAL_NAME, "test"));
    // After renaming the currentCrystal must be the new one.
    CPPUNIT_ASSERT_EQUAL(string("test"), m_diffractometer->getCurrentCrystalName());

    // The old crystal name must not be found in the crystal list.
    CPPUNIT_ASSERT_THROW(m_diffractometer->setCurrentCrystal(DEFAULT_CRYSTAL_NAME), HKLException);
}

void
Diffractometer_TwoC_Test::delCrystal(void)
{
    // When we delete the currentCrystal
    // The currentCrystal must be unset if there is more than one crystal
    // in the crystallist.
    m_diffractometer->addNewCrystal("test1");
    m_diffractometer->addNewCrystal("test2");
    m_diffractometer->setCurrentCrystal("test1");
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delCrystal("test1"));
    CPPUNIT_ASSERT_THROW(m_diffractometer->getCurrentCrystalName(), HKLException);

    // When the deleted crystal is not the currentCrystal, the currentCrystal must
    // not be unset.  
    m_diffractometer->addNewCrystal("test1");
    m_diffractometer->setCurrentCrystal("test2");
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delCrystal("test1"));
    CPPUNIT_ASSERT_EQUAL(string("test2"), m_diffractometer->getCurrentCrystalName());

    // When we remove the last crystal, the new currentCrystal must be the default one.
    m_diffractometer->delCrystal(DEFAULT_CRYSTAL_NAME);
    m_diffractometer->delCrystal("test2");
    CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), m_diffractometer->getCurrentCrystalName());
}

void
Diffractometer_TwoC_Test::delAllCrystals(void)
{
    // Add another crystal than the default one.
    m_diffractometer->addNewCrystal("test");
    m_diffractometer->setCurrentCrystal("test");
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delAllCrystals());
    // The new currentCrystal must be the first crystal in the crystal List
    // here the default one.
    CPPUNIT_ASSERT_EQUAL(string(DEFAULT_CRYSTAL_NAME), m_diffractometer->getCurrentCrystalName());
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

    m_diffractometer->addNewCrystal("crystal");

    CPPUNIT_ASSERT_THROW(m_diffractometer->getCrystalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->setCrystalLattice("toto", a, b, c, alpha, beta, gamma), HKLException);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setCrystalLattice("crystal", a, b, c, alpha, beta, gamma));

    m_diffractometer->setCrystalLattice("crystal", 1., 2., 3., 2., 1., 2.);
    m_diffractometer->getCrystalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2., b, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(3., c, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2., alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1., beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2., gamma, constant::math::epsilon_0);
}

void
Diffractometer_TwoC_Test::getCrystalParametersNames(void)
{
    m_diffractometer->addNewCrystal("crystal");

    CPPUNIT_ASSERT_THROW(m_diffractometer->getCrystalParametersNames("toto"), HKLException);

    vector<string> names;
    CPPUNIT_ASSERT_NO_THROW(names = m_diffractometer->getCrystalParametersNames("crystal"));
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

    m_diffractometer->addNewCrystal("crystal");

    CPPUNIT_ASSERT_THROW(m_diffractometer->getCrystalReciprocalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));

    m_diffractometer->setCrystalLattice("crystal", 1., 2., 3., 90. * constant::math::degToRad, 90. * constant::math::degToRad, 90. * constant::math::degToRad);
    m_diffractometer->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau, a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 2., b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 3., c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * constant::math::degToRad, gamma, constant::math::epsilon_0);
}

void
Diffractometer_TwoC_Test::AddReflection(void)
{
    m_diffractometer->addNewCrystal("crystal");

    CPPUNIT_ASSERT_THROW(m_diffractometer->addCrystalReflection("toto", 0, 0, 1, Best, true), HKLException);

    //even if the crystal exist, the wavelength must be set.
    CPPUNIT_ASSERT_THROW(m_diffractometer->addCrystalReflection("crystal",
                                                                0, 0, 1,
                                                                Best, true),
                         HKLException);

    m_diffractometer->setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true));
}

void
Diffractometer_TwoC_Test::DelReflection(void)
{
    m_diffractometer->setWaveLength(1.54);
    m_diffractometer->addNewCrystal("crystal");

    CPPUNIT_ASSERT_THROW(m_diffractometer->delCrystalReflection("toto", 0), HKLException);

    m_diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true);
    m_diffractometer->addCrystalReflection("crystal", 0, 0, 1, Best, true);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delCrystalReflection("crystal", 1));
    CPPUNIT_ASSERT_THROW(m_diffractometer->delCrystalReflection("crystal", 1), HKLException);
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->delCrystalReflection("crystal", 0));
    CPPUNIT_ASSERT_THROW(m_diffractometer->delCrystalReflection("crystal", 0), HKLException);
}

/*
   void
   diffractometerTest::GetReflection()
   {
   Diffractometer *d = new Diffractometer_Eulerian4C();

   m_diffractometer->addNewCrystal("crystal");
   m_diffractometer->setCrystal("crystal");

   CPPUNIT_ASSERT_THROW(m_diffractometer->getReflection(0), HKLException);
   CPPUNIT_ASSERT_THROW(m_diffractometer->getCrystalReflection("toto", 0), HKLException);

   m_diffractometer->addReflection(0, 0, 1, Reflection::Best, true); 

   CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getReflection(0));
   CPPUNIT_ASSERT_NO_THROW(m_diffractometer->getCrystalReflection("crystal", 0));

   delete d;
   }
   */


void
Diffractometer_TwoC_Test::ModePart(void)
{
    // Test each mode.
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setCurrentMode("Symetric"));
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->setCurrentMode("Fix incidence"));

    // try to set an unknown mode ans check if the currentMode is the last valid currentMode.
    CPPUNIT_ASSERT_THROW(m_diffractometer->setCurrentMode("toto"), HKLException);
    CPPUNIT_ASSERT_EQUAL(string("Fix incidence"), m_diffractometer->getCurrentModeName());

    // test the parameters
    CPPUNIT_ASSERT_THROW(m_diffractometer->setModeParameterValue("Symetric", "titi", 10.), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->setModeParameterValue("Fix incidence", "titi", 10.), HKLException);
}

/*
   void 
   diffractometerTest::ComputeU()
   {
   Diffractometer *d = new Diffractometer_Eulerian4C();  
   m_diffractometer->setWaveLength(1.54);
//m_diffractometer->setIncidentBeamDirection(1., 0., 0.);

m_diffractometer->addNewCrystal("crystal1");
m_diffractometer->setCurrentCrystal("crystal1");

CPPUNIT_ASSERT_THROW(m_diffractometer->computeU(), HKLException);
m_diffractometer->setCrystalLattice("crystal1",
1.54, 1.54, 1.54,
90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );


CPPUNIT_ASSERT_THROW(m_diffractometer->computeU(), HKLException);

m_diffractometer->setAxeValue("2theta", 60.*constant::math::degToRad);  
m_diffractometer->setAxeValue("omega", 30.*constant::math::degToRad);
m_diffractometer->setAxeValue("chi", 0.);
m_diffractometer->setAxeValue("phi", 90.*constant::math::degToRad);
m_diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);

CPPUNIT_ASSERT_THROW(m_diffractometer->computeU(), HKLException);

m_diffractometer->setAxeValue("phi", 180.*constant::math::degToRad);
m_diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);

CPPUNIT_ASSERT_NO_THROW(m_diffractometer->computeU());

smatrix M(1., 0., 0.,
0., 0., 1.,
0., -1., 0.);

CPPUNIT_ASSERT_EQUAL(M, m_diffractometer->getCurrentCrystal().get_U());

delete d;
}
*/


void 
Diffractometer_TwoC_Test::ComputeHKL(void)
{
    double h, k, l;

    m_diffractometer->setWaveLength(1.54);

    m_diffractometer->addNewCrystal("crystal1");
    m_diffractometer->setCurrentCrystal("crystal1");
    m_diffractometer->setCrystalLattice("crystal1", 1.54, 1.54, 1.54,
                                        90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );

    m_diffractometer->setAxeValue("2theta", 60.*constant::math::degToRad);  
    m_diffractometer->setAxeValue("omega", 30.*constant::math::degToRad);
    m_diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Best, true);

    m_diffractometer->setAxeValue("omega", 120.*constant::math::degToRad);
    m_diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Best, true);
    m_diffractometer->computeU();

    m_diffractometer->computeHKL(h, k, l);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., h, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1., k, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);

    m_diffractometer->setAxeValue("omega", 30.*constant::math::degToRad);
    m_diffractometer->computeHKL(h, k, l);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1., h, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);

    m_diffractometer->setAxeValue("2theta", 180.*constant::math::degToRad);
    m_diffractometer->setAxeValue("omega", 90.*constant::math::degToRad);
    m_diffractometer->computeHKL(h, k, l);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2., h, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, constant::math::epsilon_1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, constant::math::epsilon_1);  
}

void 
Diffractometer_TwoC_Test::ComputeAngles(void)
{
    CPPUNIT_ASSERT_THROW(m_diffractometer->computeAngles(1., 1., 1.), HKLException);

    m_diffractometer->setCurrentMode("Symetric");
    CPPUNIT_ASSERT_THROW(m_diffractometer->computeAngles(1., 1., 1.), HKLException);

    m_diffractometer->setWaveLength(1.);
    CPPUNIT_ASSERT_THROW(m_diffractometer->computeAngles(1., 1., 1.), HKLException);
    CPPUNIT_ASSERT_THROW(m_diffractometer->computeAngles(0., 0., 0.), HKLException);

    m_diffractometer->addNewCrystal("crystal1");
    m_diffractometer->setCurrentCrystal("crystal1");
    CPPUNIT_ASSERT_THROW(m_diffractometer->computeAngles(1., 1., 1.), HKLException);

    m_diffractometer->setCrystalLattice("crystal1", 1., 1., 1.,
                                        90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad );

    m_diffractometer->setAxeValue("2theta", 60.*constant::math::degToRad);  
    m_diffractometer->setAxeValue("omega", 30.*constant::math::degToRad);
    m_diffractometer->addCrystalReflection("crystal1", 1., 0., 0., Best, true);

    m_diffractometer->setAxeValue("omega", 120.*constant::math::degToRad);
    m_diffractometer->addCrystalReflection("crystal1", 0., 1., 0., Best, true);
    m_diffractometer->computeU();

    //Symetric
    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->computeAngles(1., 0., 0.));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_diffractometer->getAxeValue("2theta"), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_diffractometer->getAxeValue("omega"), constant::math::epsilon_0);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->computeAngles(0., 1., 0.));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_diffractometer->getAxeValue("2theta"), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_diffractometer->getAxeValue("omega"), constant::math::epsilon_0);

    //Fix incidence
    m_diffractometer->setCurrentMode("Fix incidence");
    m_diffractometer->setAxeValue("omega", 120.*constant::math::degToRad);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->computeAngles(1., 0., 0.));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_diffractometer->getAxeValue("2theta"), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(120*constant::math::degToRad, m_diffractometer->getAxeValue("omega"), constant::math::epsilon_0);

    CPPUNIT_ASSERT_NO_THROW(m_diffractometer->computeAngles(0., 2., 0.));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(180*constant::math::degToRad, m_diffractometer->getAxeValue("2theta"), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(120*constant::math::degToRad, m_diffractometer->getAxeValue("omega"), constant::math::epsilon_0);
}

void
Diffractometer_TwoC_Test::persistanceIO(void)
{
    diffractometer::twoC::Vertical d_ref;
    diffractometer::twoC::Vertical d;  
    diffractometer::twoC::Vertical d1_ref;
    diffractometer::twoC::Vertical d1;
    stringstream flux;

    d_ref.setWaveLength(2.43);
    d_ref.addNewCrystal("titi");
    d_ref.setCrystalLattice("titi",
                            2.34, 4.5, 2.7,
                            90*constant::math::degToRad, 120*constant::math::degToRad, 60*constant::math::degToRad);
    d_ref.setCurrentMode("Symetric");
    d_ref.setCurrentCrystal("titi");

    // Reflection 1
    d_ref.setAxeValue("2theta", 30.398*constant::math::degToRad);
    d_ref.setAxeValue("omega", 11.709*constant::math::degToRad);
    d_ref.addCrystalReflection("titi", 0., 0., 1., Best, true);

    // Reflection 2
    d_ref.setAxeValue("2theta", 21.001*constant::math::degToRad);
    d_ref.setAxeValue("omega", 10.322*constant::math::degToRad);
    d_ref.addCrystalReflection("titi", 0., 2., 0., Best, true);

    // Reflection 3
    d_ref.setAxeValue("2theta", 54.046*constant::math::degToRad);
    d_ref.setAxeValue("omega", 26.872*constant::math::degToRad);
    d_ref.addCrystalReflection("titi", -2., 2., 1., Best, true);

    // Reflection 4
    d_ref.setAxeValue("2theta", 37.333*constant::math::degToRad);
    d_ref.setAxeValue("omega", 18.51*constant::math::degToRad);
    d_ref.addCrystalReflection("titi", -1., 1., 1., Best, true);

    d_ref.toStream(flux);
    d1_ref.toStream(flux);
    d.fromStream(flux);
    d1.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(d_ref, d);
    CPPUNIT_ASSERT_EQUAL(d1_ref, d1);
}
