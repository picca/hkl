// File to test matrix and vector implementation.
#include "lattice_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( LatticeTest );

void
LatticeTest::setUp()
{
  m_tau = physicalConstants::getTau();
  m_epsilon = mathematicalConstants::getEpsilon1();
  m_degToRad = mathematicalConstants::convertAnglesToRadians();
}

void 
LatticeTest::tearDown() 
{
}

void
LatticeTest::Constructor1()
{
  Lattice lattice;
  
  CPPUNIT_ASSERT_EQUAL(1., lattice.get_a());
  CPPUNIT_ASSERT_EQUAL(1., lattice.get_b());
  CPPUNIT_ASSERT_EQUAL(1., lattice.get_c());
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, lattice.get_alpha());
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, lattice.get_beta());
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, lattice.get_gamma());
}

void 
LatticeTest::Constructor2()
{
  Lattice lattice(1.54, 1.54, 1.54,
                  60.0*m_degToRad, 60.0*m_degToRad, 60.0*m_degToRad);
  
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_a());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_b());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_c());
  CPPUNIT_ASSERT_EQUAL(60.*m_degToRad, lattice.get_alpha());
  CPPUNIT_ASSERT_EQUAL(60.*m_degToRad, lattice.get_beta());
  CPPUNIT_ASSERT_EQUAL(60.*m_degToRad, lattice.get_gamma());
}

void
LatticeTest::Equal()
{
  Lattice lattice1;
  Lattice lattice2(1.54, 1.54, 1.54,
                   60.0*m_degToRad, 60.0*m_degToRad, 60.0*m_degToRad);
  
  CPPUNIT_ASSERT_EQUAL(lattice1, lattice1);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(lattice1, lattice2));
}

void
LatticeTest::CopyConstructor()
{ 
  Lattice lattice1(1.54, 1.54, 1.54,
                   60.0*m_degToRad, 60.0*m_degToRad, 60.0*m_degToRad);
  Lattice lattice2(lattice1);
  
  CPPUNIT_ASSERT_EQUAL(lattice1, lattice2);
}

void
LatticeTest::GetSet()
{
  Lattice lattice;
  Lattice l;
  
  lattice.set_a(1.54);
  lattice.set_b(1.54);
  lattice.set_c(1.54);
  lattice.set_alpha(91.*m_degToRad);
  lattice.set_beta(89.*m_degToRad);
  lattice.set_gamma(60.*m_degToRad);
    
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_a());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_b());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.get_c());
  CPPUNIT_ASSERT_EQUAL(91.*m_degToRad, lattice.get_alpha());
  CPPUNIT_ASSERT_EQUAL(89.*m_degToRad, lattice.get_beta());
  CPPUNIT_ASSERT_EQUAL(60.*m_degToRad, lattice.get_gamma());
  
  lattice.set(1., 1., 1., 90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  CPPUNIT_ASSERT_EQUAL(l, lattice);
  
  double a, b, c, alpha, beta, gamma;
  lattice.get(&a, &b, &c, &alpha, &beta, &gamma);
  
  CPPUNIT_ASSERT_EQUAL(1., a);
  CPPUNIT_ASSERT_EQUAL(1., b);
  CPPUNIT_ASSERT_EQUAL(1., c);
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, alpha);
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, beta);
  CPPUNIT_ASSERT_EQUAL(90.*m_degToRad, gamma);
}

void
LatticeTest::ComputeReciprocalLattice()
{
  // cubic
  Lattice lattice(1.54, 1.54, 1.54,
                  90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  Lattice reciprocalLattice(m_tau/1.54, m_tau/1.54, m_tau/1.54,
                            90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice());
  
  //orthorombic
  lattice.set(1., 3., 4.,
              90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  reciprocalLattice.set(m_tau/1., m_tau/3., m_tau/4.,
                        90.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice());
  
  // hexagonal1
  lattice.set(1., 2., 1.,
              90.*m_degToRad, 120.*m_degToRad, 90.*m_degToRad);
  reciprocalLattice.set(m_tau*2./sqrt(3.), m_tau/2., m_tau*2./sqrt(3.),
                        90.*m_degToRad, 60.*m_degToRad, 90.*m_degToRad);
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice()); 
  
  // hexagonal2
  lattice.set(2., 1., 1.,
              120.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  reciprocalLattice.set(m_tau/2., m_tau*2./sqrt(3.), m_tau*2./sqrt(3.),
                        60.*m_degToRad, 90.*m_degToRad, 90.*m_degToRad);
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice()); 
  
  // triclinic1
  lattice.set(9.32, 8.24, 13.78,
              91.23*m_degToRad, 93.64*m_degToRad, 122.21*m_degToRad);
  reciprocalLattice.set(m_tau*0.1273130168, m_tau*0.1437422974, m_tau*0.0728721120,
                        1.5052513337, 1.482101482, 1.0055896011);
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice());
  
  // triclinic2
  lattice.set(18.423, 18.417, 18.457,
              89.99*m_degToRad, 89.963*m_degToRad, 119.99*m_degToRad);
  reciprocalLattice.set(m_tau*0.0626708259, m_tau*0.0626912310, m_tau*0.0541800061,
                        1.5713705262, 1.5716426508, 1.0473718249);
  CPPUNIT_ASSERT_EQUAL(reciprocalLattice, lattice.computeReciprocalLattice());
}
