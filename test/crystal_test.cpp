#include "crystal_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( CrystalTest );

void
CrystalTest::setUp()
{
  m_crystal.setLattice(1.54, 1.54, 1.54,
                       90.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
  
  m_source.setWaveLength(1.54);
}

void 
CrystalTest::tearDown() 
{
}

void
CrystalTest::Constructor()
{
  Crystal C("cristal");
  smatrix m;
  
  CPPUNIT_ASSERT_EQUAL(std::string("cristal"), C.get_name());
  CPPUNIT_ASSERT_EQUAL(m, C.get_B());
  CPPUNIT_ASSERT_EQUAL(smatrix(1., 0., 0., 0., 1., 0., 0., 0., 1.), C.get_U());
}

void
CrystalTest::Equal()
{
  Crystal C1("crystal1");
  Crystal C2("crystal2");
  
  CPPUNIT_ASSERT_EQUAL(C1, C1);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(C1, C2));
}

void
CrystalTest::CopyConstructor()
{ 
  Crystal C1("crystal1");
  Crystal C2(C1);

  CPPUNIT_ASSERT_EQUAL(C1, C2);
}

void
CrystalTest::GetLattice()
{
  Crystal crystal("crystal1");
  double a, b, c, alpha, beta, gamma;
  
  crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_EQUAL(0., a);
  CPPUNIT_ASSERT_EQUAL(0., b);
  CPPUNIT_ASSERT_EQUAL(0., c);
  CPPUNIT_ASSERT_EQUAL(0., alpha);
  CPPUNIT_ASSERT_EQUAL(0., beta);
  CPPUNIT_ASSERT_EQUAL(0., gamma);
}

void
CrystalTest::SetLattice()
{
  Crystal crystal("crystal1");
  double a, b, c, alpha, beta, gamma;
  
  crystal.setLattice(1., 1., 1.54, 3., 2., 1.);
  crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_EQUAL(1., a);
  CPPUNIT_ASSERT_EQUAL(1., b);
  CPPUNIT_ASSERT_EQUAL(1.54, c);
  CPPUNIT_ASSERT_EQUAL(3., alpha);
  CPPUNIT_ASSERT_EQUAL(2., beta);
  CPPUNIT_ASSERT_EQUAL(1., gamma);
}

void
CrystalTest::GetReciprocalLattice()
{
  Crystal crystal("crystal1");
  double a, b, c, alpha, beta, gamma;
  
  // cubic
  crystal.setLattice(1.54, 1.54, 1.54,
                     90.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/1.54, a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/1.54, b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/1.54, c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);
  
  //orthorombic
  crystal.setLattice(1., 3., 4.,
                     90.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/1., a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/3., b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/4., c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);
  
  // hexagonal1
  crystal.setLattice(1., 2., 1.,
                     90.* constant::math::degToRad, 120.* constant::math::degToRad, 90.* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*2./sqrt(3.), a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/2., b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*2./sqrt(3.), c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60. *  constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);
  
  // hexagonal2
  crystal.setLattice(2., 1., 1.,
                     120.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau/2., a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*2./sqrt(3.), b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*2./sqrt(3.), c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60.* constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.* constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.* constant::math::degToRad, gamma, constant::math::epsilon_0);
  
  // triclinic1
  crystal.setLattice(9.32, 8.24, 13.78,
                     91.23* constant::math::degToRad, 93.64* constant::math::degToRad, 122.21* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.1273130168, a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.1437422974, b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.0728721120, c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5052513337, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.482101482, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.0055896011, gamma, constant::math::epsilon_0);
  
  // triclinic2
  crystal.setLattice(18.423, 18.417, 18.457,
                     89.99* constant::math::degToRad, 89.963* constant::math::degToRad, 119.99* constant::math::degToRad);
  crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.0626708259, a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.0626912310, b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau*0.0541800061, c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5713705262, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5716426508, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.0473718249, gamma, constant::math::epsilon_0);
}

void
CrystalTest::PlusEqual(void)
{
  double a, b, c, alpha, beta, gamma;

  Crystal C1("crystal1");
  Crystal C2("crystal2");
  C1.setLattice(1., 1., 1., 1., 2., 3.);
  C2.setLattice(2., 3., 4., 5., 6., 7.);
  
  C1 += C2;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_EQUAL(3., a);
  CPPUNIT_ASSERT_EQUAL(4., b);
  CPPUNIT_ASSERT_EQUAL(5., c);
  CPPUNIT_ASSERT_EQUAL(6., alpha);
  CPPUNIT_ASSERT_EQUAL(8., beta);
  CPPUNIT_ASSERT_EQUAL(10., gamma);

  C1 += C1;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_EQUAL(6., a);
  CPPUNIT_ASSERT_EQUAL(8., b);
  CPPUNIT_ASSERT_EQUAL(10., c);
  CPPUNIT_ASSERT_EQUAL(12., alpha);
  CPPUNIT_ASSERT_EQUAL(16., beta);
  CPPUNIT_ASSERT_EQUAL(20., gamma);
}

void
CrystalTest::MinusEqual(void)
{
  double a, b, c, alpha, beta, gamma;

  Crystal C1("crystal1");
  Crystal C2("crystal2");
  C1.setLattice(1., 1., 1., 1., 2., 3.);
  C2.setLattice(2., 3., 4., 5., 6., 7.);
  
  C1 -= C2;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_EQUAL(-1., a);
  CPPUNIT_ASSERT_EQUAL(-2., b);
  CPPUNIT_ASSERT_EQUAL(-3., c);
  CPPUNIT_ASSERT_EQUAL(-4., alpha);
  CPPUNIT_ASSERT_EQUAL(-4., beta);
  CPPUNIT_ASSERT_EQUAL(-4., gamma);

  C1 -= C1;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_EQUAL(0., a);
  CPPUNIT_ASSERT_EQUAL(0., b);
  CPPUNIT_ASSERT_EQUAL(0., c);
  CPPUNIT_ASSERT_EQUAL(0., alpha);
  CPPUNIT_ASSERT_EQUAL(0., beta);
  CPPUNIT_ASSERT_EQUAL(0., gamma);
}

void
CrystalTest::TimesEqual(void)
{
  double a, b, c, alpha, beta, gamma;

  Crystal C1("crystal1");
  C1.setLattice(1., 1., 1., 1., 2., 3.);
  
  C1 *= 2.;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_EQUAL(2., a);
  CPPUNIT_ASSERT_EQUAL(2., b);
  CPPUNIT_ASSERT_EQUAL(2., c);
  CPPUNIT_ASSERT_EQUAL(2., alpha);
  CPPUNIT_ASSERT_EQUAL(4., beta);
  CPPUNIT_ASSERT_EQUAL(6., gamma);
}

void
CrystalTest::DivideEqual(void)
{
  double a, b, c, alpha, beta, gamma;

  Crystal C1("crystal1");
  C1.setLattice(1., 1., 1., 1., 2., 3.);
  
  C1 /= 2.;
  C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_EQUAL(.5, a);
  CPPUNIT_ASSERT_EQUAL(.5, b);
  CPPUNIT_ASSERT_EQUAL(.5, c);
  CPPUNIT_ASSERT_EQUAL(.5, alpha);
  CPPUNIT_ASSERT_EQUAL(1., beta);
  CPPUNIT_ASSERT_EQUAL(1.5, gamma);
}

void
CrystalTest::ReflectionPart()
{
  Crystal crystal("crystal1");
  
  //add
  
  crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                   0.,0.,1.,
                                   Reflection::Best, true));
  crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                   0.,0.,1.,
                                   Reflection::Best, true));
  
  // get
  CPPUNIT_ASSERT_NO_THROW(crystal.getReflection(1));
  CPPUNIT_ASSERT_NO_THROW(crystal.getReflection(0));
  CPPUNIT_ASSERT_THROW(crystal.getReflection(2), HKLException);
  
  //del
  CPPUNIT_ASSERT_NO_THROW(crystal.delReflection(1));
  CPPUNIT_ASSERT_NO_THROW(crystal.delReflection(0));
  CPPUNIT_ASSERT_THROW(crystal.delReflection(0), HKLException);
}

void
CrystalTest::ComputeB()
{
  smatrix matrice(constant::physic::tau/1.54,         0.,         0.,
                          0., constant::physic::tau/1.54,         0.,
                          0.,         0., constant::physic::tau/1.54);
  
  CPPUNIT_ASSERT_EQUAL(matrice, m_crystal.get_B());
}

void
CrystalTest::isEnoughReflections(void)
{
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     0., 0., 1.,
                                     Reflection::Best, true));
  
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     -1., 0., 0.,
                                     Reflection::Best, true));
  
  CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(0));
  CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(1));
  CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(2));
  CPPUNIT_ASSERT_EQUAL(false, m_crystal.isEnoughReflections(3));
}

void
CrystalTest::ComputeU()
{
  smatrix M(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);
  
  CPPUNIT_ASSERT_THROW(m_crystal.computeU(), HKLException);
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     0.,0.,1.,
                                     Reflection::Best, true));
  
  CPPUNIT_ASSERT_THROW(m_crystal.computeU(), HKLException);
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(-90.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     -1.,0.,0.,
                                     Reflection::Best, true));

  CPPUNIT_ASSERT_NO_THROW(m_crystal.computeU());
  CPPUNIT_ASSERT_EQUAL(M, m_crystal.get_U());

  m_crystal.delReflection(1);
  CPPUNIT_ASSERT_THROW(m_crystal.computeU(), HKLException);
  m_crystal.delReflection(0);
  CPPUNIT_ASSERT_THROW(m_crystal.computeU(), HKLException);
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(90.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     1.,0.,0.,
                                     Reflection::Best, true));
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(180.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     0.,1.,0.,
                                     Reflection::Best, true));
  m_crystal.computeU();
 
  M.set(1., 0., 0.,
        0., 0., 1.,
        0.,-1., 0.);

  CPPUNIT_ASSERT_EQUAL(M, m_crystal.get_U());
}

void
CrystalTest::Fitness()
{
  smatrix M(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     0., 0., 1.,
                                     Reflection::Best, true));
  
  m_geometry_E4C.get_axe("2theta").set_value(60.* constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.* constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.* constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(-90.* constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     -1., 0., 0.,
                                     Reflection::Best, true));
  
  m_crystal.computeU();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., m_crystal.fitness(), constant::math::epsilon_1);
}
