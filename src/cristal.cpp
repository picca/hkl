// Cristal class functions to define the different lattices.

#include "cristal.h"
#include "constants.h"
#include <iostream.h>
#include <math.h>


// The arguments define both the direct and reciprocal 
// lattices. a1, a2, a3 define the direct lattice lengths
// and alpha1, alpha2, alpha3 the angles between the 
// lattice vectors. Similarly b1, b2, b3 define the direct
// lattice lengths and beta1, beta2, beta3 the angles.
cristal::cristal(
    double alpha1, double alpha2, double alpha3,
    double beta1, double beta2, double beta3,
    double a1, double a2, double a3, 
    double b1, double b2, double b3)
{
  m_alpha1 = alpha1;
  m_alpha2 = alpha2;
  m_alpha3 = alpha3;
  m_beta1 = beta1;
  m_beta2 = beta2;
  m_beta3 = beta3;
  m_a1 = a1;
  m_a2 = a2;
  m_a3 = a3;
  m_b1 = b1;
  m_b2 = b2;
  m_b3 = b3;
  computeB();
}

// The arguments define the direct lattice. a1, a2, a3 
// define the direct lattice lengths and alpha1, alpha2,
// alpha3 the angles between the lattice vectors. The
// reciprocal parameters are computed from the 6 arguments.
cristal::cristal(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3)
{
  m_alpha1 = alpha1;
  m_alpha2 = alpha2;
  m_alpha3 = alpha3;
  m_a1 = a1;
  m_a2 = a2;
  m_a3 = a3;
  computeReciprocalLattice();
  computeB();
}

cristal::cristal(const cristal& C)
{
  m_alpha1 = C.m_alpha1;
  m_alpha2 = C.m_alpha2;
  m_alpha3 = C.m_alpha3;
  m_beta1 = C.m_beta1;
  m_beta2 = C.m_beta2;
  m_beta3 = C.m_beta3;
  m_a1 = C.m_a1;
  m_a2 = C.m_a2;
  m_a3 = C.m_a3;
  m_b1 = C.m_b1;
  m_b2 = C.m_b2;
  m_b3 = C.m_b3;
  m_B.set(C.m_B);
}

// William R. Busing and Henri A. Levy "Angle calculation 
// for 3- and 4- Circle X-ray and Neutron Diffractometer"
// (1967) Acta Cryst., 22, 457-464.
// Compute the matrix B from equation (3) page 458.
void cristal::computeB()
{
  m_B.set(
    m_b1, m_b2 * cos(m_beta3), m_b3 * cos(m_beta2),
    0.,m_b2*sin(m_beta3),-m_b3*sin(m_beta2)*cos(m_alpha1),
    0., 0., physicalConstants::getTau() / m_a3);
}

// A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
// By Finite and Imperfect Crystals" (1962)
// John Wiley & Sons Inc., 14-17.
void cristal::computeReciprocalLattice()
{
  double D = sqrt( 1 
    - cos(m_alpha1)*cos(m_alpha1) 
    - cos(m_alpha2)*cos(m_alpha2)
    - cos(m_alpha3)*cos(m_alpha3)
    + 2*cos(m_alpha1)*cos(m_alpha2)*cos(m_alpha3));
  m_b1 = (sin(m_alpha1) / (m_a1 * D));
  m_b2 = (sin(m_alpha2) / (m_a2 * D));
  m_b3 = (sin(m_alpha3) / (m_a3 * D));

  double cos_beta1 =
    (cos(m_alpha2)*cos(m_alpha3) - cos(m_alpha1)) /
    (sin(m_alpha2)*sin(m_alpha3));
  double cos_beta2 = 
    (cos(m_alpha3)*cos(m_alpha1) - cos(m_alpha2)) /
    (sin(m_alpha3)*sin(m_alpha1));
  double cos_beta3 = 
    (cos(m_alpha1)*cos(m_alpha2) - cos(m_alpha3)) /
    (sin(m_alpha1)*sin(m_alpha2));
  double sin_beta1 = D / (sin(m_alpha2) * sin(m_alpha3));
  double sin_beta2 = D / (sin(m_alpha3) * sin(m_alpha1));
  double sin_beta3 = D / (sin(m_alpha2) * sin(m_alpha1));

  // The constant tau 
  m_b1 = physicalConstants::getTau() * m_b1;
  m_b2 = physicalConstants::getTau() * m_b2;
  m_b3 = physicalConstants::getTau() * m_b3;

  m_beta1 = atan2(sin_beta1, cos_beta1);
  m_beta2 = atan2(sin_beta2, cos_beta2);
  m_beta3 = atan2(sin_beta3, cos_beta3);
}


void cristal::printOnScreen() const
{
  //cout.precision(20);
  cout << endl << "CLASS cristal";
  cout << endl << "Direct lattice";
  cout << endl << 
    "alpha1=" << m_alpha1 << '\t' <<
    "alpha2=" << m_alpha2 << '\t' <<
    "alpha3=" << m_alpha3 << endl;
  cout << endl << 
    "a1=" << m_a1 << '\t' <<
    "a2=" << m_a2 << '\t' <<
    "a3=" << m_a3 << endl;
  cout << endl << "Reciprocal lattice";
  cout << endl << 
    "beta1=" << m_beta1 << '\t' <<
    "beta2=" << m_beta2 << '\t' <<
    "beta3=" << m_beta3 << endl;
  cout << endl << 
    "b1=" << m_b1 << '\t' <<
    "b2=" << m_b2 << '\t' <<
    "b3=" << m_b3 << endl;
  m_B.printOnScreen();
}

int cristal::check_cristal(const smatrix& B) const
{
  if (fabs(B.get(1,1) - m_B.get(1,1)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(1,2) - m_B.get(1,2)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(1,3) - m_B.get(1,3)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(2,1) - m_B.get(2,1)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(2,2) - m_B.get(2,2)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(2,3) - m_B.get(2,3)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(3,1) - m_B.get(3,1)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(3,2) - m_B.get(3,2)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  if (fabs(B.get(3,3) - m_B.get(3,3)) > 
    mathematicalConstants::getEpsilon0())
    return -1;
  return 0;
}

// In this function we compare theoritical values with 
// what we get from the implementation. Note that it is
// meaningful only when the convention tau = 1 has been
// choisen (i.e. tau <> 2PI).
int cristal::test_cristals()
{
  /*
  cout << endl;
  cout << "******************" << endl;
  cout << "***** CUBIC *****" << endl;
  cout << "****************" << endl;
  */
  cristal cubic_cristal1(
    1.5707963267948966,1.5707963267948966,1.5707963267948966,
    1.54, 1.54, 1.54);
  smatrix B(
    0.649350649350649,  0.,                 0.,
    0.,                 0.649350649350649,  0.,
    0.,                 0.,                 0.649350649350649);
  if (cubic_cristal1.check_cristal(B) == -1)
    return 1;


  /*
  cout << endl;
  cout << "************************" << endl;
  cout << "***** ORTHOROMBIC *****" << endl;
  cout << "**********************" << endl;
  */
  cristal orthorombic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.,3.,4.);
  B.set(
    1.,       0.,                 0.,
    0.,       0.333333333333333,  0.,
    0.,       0.,                 0.25);
  if (orthorombic_cristal1.check_cristal(B) == -1)
    return 2;

   /*
  cout << endl;
  cout << "**********************" << endl;
  cout << "***** HEXAGONAL *****" << endl;
  cout << "********************" << endl;
  */
  cristal hexagonal_cristal1(
    mathematicalConstants::getPI() / 2.,
    2. * mathematicalConstants::getPI() / 3.,
    mathematicalConstants::getPI() / 2.,
    1.,2.,1.);
  B.set(
    1.15470053837925, 0.,   0.577350269189625,
    0.,               0.5,  0.,
    0.,               0.,   1.);
  if (hexagonal_cristal1.check_cristal(B) == -1)
    return 3;

   /*
  cout << endl;
  cout << "**********************" << endl;
  cout << "***** HEXAGONAL *****" << endl;
  cout << "********************" << endl;
  */
  cristal hexagonal_cristal2(
    mathematicalConstants::getPI() * 2./ 3.,
    mathematicalConstants::getPI() / 2.,
    mathematicalConstants::getPI() / 2.,
    2.,1.,1.);
  B.set(
    0.5,  0.,               0.,
    0.,   1.15470053837925, 0.577350269189625,
    0.,   0.,               1.);
  if (hexagonal_cristal2.check_cristal(B) == -1)
    return 4;

   /*
  cout << endl;
  cout << "**********************" << endl;
  cout << "***** TRICLINIC *****" << endl;
  cout << "********************" << endl;
  */
  cristal triclinic_cristal1(
    91.230 * mathematicalConstants::getPI() / 180.,
    93.640 * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
    //9.32 / (2 * PI), 8.24 / (2 * PI), 13.78 / (2 * PI));
  B.set(
    0.127313016797711,  0.0769869949676397, 0.00645490967425875,
    0.,                 0.121387193216382,  0.00155811670694921,
    0.,                 0.,                 0.0725689404934688);
  if (triclinic_cristal1.check_cristal(B) == -1)
    return 5;

   /*
  cout << endl;
  cout << "**********************" << endl;
  cout << "***** TRICLINIC *****" << endl;
  cout << "********************" << endl;
   */
  cristal triclinic_cristal2(
    89.990 * mathematicalConstants::getPI() / 180.,
    89.963 * mathematicalConstants::getPI() / 180.,
    119.99 * mathematicalConstants::getPI() / 180.,
    18.423, 18.417, 18.457);
    //18.423 / (2 * PI), 18.417 / (2 * PI), 18.457 / (2 * PI));
  B.set(
    0.0626708259185456, 0.0313361533617482, -4.58538358482623e-005,
    0.,                 0.0542976605978645, -9.45619152472762e-006,
    0.,                 0.,                 0.0541799859132037);
  if (triclinic_cristal2.check_cristal(B) == -1)
    return 6;

  return 0;
}
