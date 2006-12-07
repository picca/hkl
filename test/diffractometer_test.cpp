// File to test quaternion implementation.
#include "diffractometer_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( diffractometerTest );

void
diffractometerTest::setUp()
{
  m_epsilon = mathematicalConstants::getEpsilon0();
  m_degToRad = mathematicalConstants::convertAnglesToRadians();
}

void
diffractometerTest::tearDown()
{}

void
diffractometerTest::GetSetAxe()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

  CPPUNIT_ASSERT_THROW(d->setAxeAngle("nu", 0.), HKLException);
  CPPUNIT_ASSERT_THROW(d->getAxeAngle("nu"), HKLException);

  CPPUNIT_ASSERT_NO_THROW(d->getAxeAngle("2theta"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeAngle("omega"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeAngle("chi"));
  CPPUNIT_ASSERT_NO_THROW(d->getAxeAngle("phi"));

  CPPUNIT_ASSERT_NO_THROW(d->setAxeAngle("2theta", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeAngle("omega", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeAngle("chi", 1.));
  CPPUNIT_ASSERT_NO_THROW(d->setAxeAngle("phi", 1.));

  CPPUNIT_ASSERT_EQUAL(1., d->getAxeAngle("2theta"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeAngle("omega"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeAngle("chi"));
  CPPUNIT_ASSERT_EQUAL(1., d->getAxeAngle("phi"));

  delete d;
}

void
diffractometerTest::CrystalPart()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

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
diffractometerTest::GetSetLattice()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

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

  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., b, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(3., c, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., alpha, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., beta, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., gamma, m_epsilon);

  delete d;
}

void
diffractometerTest::GetReciprocalLattice()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();
  double a, b, c, alpha, beta, gamma;

  d->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(d->getCrystalReciprocalLattice("toto", &a, &b, &c, &alpha, &beta, &gamma), HKLException);

  CPPUNIT_ASSERT_NO_THROW(d->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma));

  d->setCrystalLattice("crystal", 1., 2., 3., 90. * m_degToRad, 90. * m_degToRad, 90. * m_degToRad);
  d->getCrystalReciprocalLattice("crystal", &a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., a, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(.5, b, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1./3., c, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * m_degToRad, alpha, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * m_degToRad, beta, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. * m_degToRad, gamma, m_epsilon);


  delete d;
}

void
diffractometerTest::AddReflection()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

  d->addNewCrystal("crystal");

  CPPUNIT_ASSERT_THROW(d->addCrystalReflection("toto", 0, 0, 1, Reflection::Best, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(d->addCrystalReflection("crystal", 0, 0, 1, Reflection::Best, true));

  delete d;
}

void
diffractometerTest::DelReflection()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

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
diffractometerTest::ModePart()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Bissector"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Delta Theta"));
  CPPUNIT_ASSERT_NO_THROW(d->setCurrentMode("Constant Omega"));
  CPPUNIT_ASSERT_THROW(d->setCurrentMode("toto"), HKLException);

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
                       90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad );
 
  
  CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
  
  d->setAxeAngle("2theta", 60.*m_degToRad);  
  d->setAxeAngle("omega", 30.*m_degToRad);
  d->setAxeAngle("chi", 0.);
  d->setAxeAngle("phi", 90.*m_degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);
  
  CPPUNIT_ASSERT_THROW(d->computeU(), HKLException);
  
  d->setAxeAngle("phi", 180.*m_degToRad);
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
diffractometerTest::ComputeHKL()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();
  double h, k, l;

  d->setWaveLength(1.54);
  //d->setIncidentBeamDirection(1., 0., 0.);

  d->addNewCrystal("crystal1");
  d->setCurrentCrystal("crystal1");
  d->setCrystalLattice("crystal1", 1.54, 1.54, 1.54,
                       90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad );

  d->setAxeAngle("2theta", 60.*m_degToRad);
  d->setAxeAngle("omega", 30.*m_degToRad);
  d->setAxeAngle("chi", 0.);
  d->setAxeAngle("phi", 90.*m_degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);

  d->setAxeAngle("phi", 180.*m_degToRad);
  d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  d->computeHKL(&h, &k, &l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., h, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., k, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, m_epsilon);

  d->setAxeAngle("phi", 90.*m_degToRad);
  d->computeHKL(&h, &k, &l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1., h, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, m_epsilon);

  d->setAxeAngle("2theta", 180.*m_degToRad);
  d->setAxeAngle("omega", 90.*m_degToRad);
  d->computeHKL(&h, &k, &l);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2., h, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., k, m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., l, m_epsilon);

  delete d;
}

void
diffractometerTest::ComputeAngles()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

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
                       90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad );

  d->setAxeAngle("2theta", 60.*m_degToRad);
  d->setAxeAngle("omega", 30.*m_degToRad);
  d->setAxeAngle("chi", 0.);
  d->setAxeAngle("phi", 90.*m_degToRad);
  d->addCrystalReflection("crystal1", 1., 0., 0., Reflection::Best, true);

  d->setAxeAngle("phi", 180.*m_degToRad);
  d->addCrystalReflection("crystal1", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  CPPUNIT_ASSERT_NO_THROW(d->computeAngles(1., 0., 0.));

  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*m_degToRad, d->getAxeAngle("2theta"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*m_degToRad, d->getAxeAngle("omega"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, d->getAxeAngle("chi"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*m_degToRad, d->getAxeAngle("phi"), m_epsilon);

  delete d;
}

void
diffractometerTest::LPS()
{
  Diffractometer *d = new Diffractometer_Eulerian4C();

  d->setCurrentMode("Bissector");

  d->setWaveLength(1.542);
  //d->setIncidentBeamDirection(1., 0., 0.);

  d->addNewCrystal("orthorombique");
  d->setCurrentCrystal("orthorombique");

  d->setCrystalLattice("orthorombique",
                       4.81, 8.47, 2.941,
                       90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad );

  d->setAxeAngle("2theta", 30.391991*m_degToRad);
  d->setAxeAngle("omega", 15.195995*m_degToRad);
  d->setAxeAngle("chi", 90.*m_degToRad);
  d->setAxeAngle("phi", 0.*m_degToRad);
  d->addCrystalReflection("orthorombique", 0., 0., 1., Reflection::Best, true);

  d->setAxeAngle("2theta", 10.445403*m_degToRad);
  d->setAxeAngle("omega", 5.2227013*m_degToRad);
  d->setAxeAngle("chi", 0.*m_degToRad);
  d->setAxeAngle("phi", 0.*m_degToRad);
  d->addCrystalReflection("orthorombique", 0., 1., 0., Reflection::Best, true);
  d->computeU();

  CPPUNIT_ASSERT_NO_THROW(d->computeAngles(0., 1., 0.));

  CPPUNIT_ASSERT_DOUBLES_EQUAL(10.445403*m_degToRad, d->getAxeAngle("2theta"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(5.2227013*m_degToRad, d->getAxeAngle("omega"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*m_degToRad, d->getAxeAngle("chi"), m_epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*m_degToRad, d->getAxeAngle("phi"), m_epsilon);
  delete d;
}
