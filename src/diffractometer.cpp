// Class diffractometer to drive experiments. Reference : 
// William R. Busing and Henri A. Levy "Angle calculation 
// for 3- and 4- Circle X-ray and Neutron Diffractometer"
// (1967) Acta Cryst., 22, 457-464.
#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "constants.h"
#include "reflection.h"
#include "angleconfig.h"
#include "HKLException.h"
#include "diffractometer.h"
#include <iostream.h>
#include <math.h>


// *********************************
// * Abstract type diffractometer *
// *******************************
diffractometer::diffractometer(
  cristal currentCristal,
  source currentSource,
  reflection& reflection1,
  reflection& reflection2):m_sizeOfArray(100),
    m_numberOfInsertedElements(0),
    m_currentCristal(currentCristal),
    m_currentSource(currentSource)
{
  m_reflectionList = new reflection[m_sizeOfArray];

  // This class owns its own memory so make sure we 
  // duplicate both the reflections and mode.
  angleConfiguration* ac = 
    reflection1.getAngleConfiguration();
  setReflection(ac,
    reflection1.get_h(),
    reflection1.get_k(),
    reflection1.get_l(), 
    reflection1.getRelevance(),
    m_numberOfInsertedElements);
  ac = reflection2.getAngleConfiguration();
  // Incrementing m_numberOfInsertedElements is done
  // inside setReflection().
  //m_numberOfInsertedElements++;
  setReflection(ac,
    reflection2.get_h(),
    reflection2.get_k(),
    reflection2.get_l(), 
    reflection2.getRelevance(),
    m_numberOfInsertedElements);
  //m_numberOfInsertedElements++;

  m_currentConfiguration = 0;
}

diffractometer::diffractometer(
  cristal currentCristal,
  source currentSource):m_sizeOfArray(100),
    m_numberOfInsertedElements(0),
    m_currentCristal(currentCristal),
    m_currentSource(currentSource)
{
  m_reflectionList = new reflection[m_sizeOfArray];

}

diffractometer::~diffractometer()
{
  delete m_currentMode;
  delete [] m_reflectionList;
}

void diffractometer::printOnScreen() const
{
  int i;
  cout << endl << "CLASS diffractometer";
  m_currentCristal.printOnScreen();
  m_currentSource.printOnScreen();

  for (i=0; i<m_numberOfInsertedElements; i++)
  {
    cout << endl << "CLASS reflection";
    cout << endl
      << "h = " << getReflection_h(i) << '\t'
      << "k = " << getReflection_k(i) << '\t'
      << "l = " << getReflection_l(i) << '\t'
      << "relevance = " << getReflection_Relevance(i);
    getReflection_AngleConfiguration(i)->printOnScreen();
  }

  cout << endl << "Matrix U";
  m_U.printOnScreen();
  cout << endl << "Matrix UB";
  m_UB.printOnScreen();
}

int diffractometer::getReflection_h(int i) const
{
  return m_reflectionList[i].get_h();
}

int diffractometer::getReflection_k(int i) const
{
  return m_reflectionList[i].get_k();
}

int diffractometer::getReflection_l(int i) const
{
  return m_reflectionList[i].get_l();
}

void diffractometer::setReflection(
  angleConfiguration* ac,
  int h, int k, int l, reflection::relevance r, int index)
{
  // reflection::set() is going to duplicate
  // ac to make sure we do not share memory.
  m_reflectionList[index].set(ac,h,k,l,r);
  m_numberOfInsertedElements++;
}

reflection::relevance
  diffractometer::getReflection_Relevance(int index) const
{
  return m_reflectionList[index].getRelevance();
}

angleConfiguration*
  diffractometer::getReflection_AngleConfiguration(
  int index) const
{
  return m_reflectionList[index].getAngleConfiguration();
}

// *******************************
// * Eulerian 4C diffractometer *
// *****************************
eulerianDiffractometer4C::eulerianDiffractometer4C(
  cristal currentCristal, source currentSource,
  reflection& reflection1, reflection& reflection2,
  mode::diffractometer_mode currentMode) : diffractometer(
    currentCristal, currentSource,
    reflection1, reflection2)
{
  //smatrix m_U;
  //smatrix m_UB;

  // Compute UB matrix.
  computeU(reflection1, reflection2);
  // Already done in computeU().
  //m_UB.set(m_U);
  //m_UB.multiplyOnTheRight(m_currentCristal.get_B());

  m_currentMode = 0;
  if (currentMode == mode::diffractometer_mode::bissector)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_bissectorMode4C();
  }
}

// Designed for testing, U = unit matrix or any other
// arbitrarily set.
eulerianDiffractometer4C::eulerianDiffractometer4C(
  cristal currentCristal, source currentSource,
  mode::diffractometer_mode currentMode) : diffractometer(
    currentCristal, currentSource)
{
  m_currentMode = 0;
  if (currentMode == mode::diffractometer_mode::bissector)
    m_currentMode = new eulerian_bissectorMode4C();

  // Compute UB for U = unity.
  m_UB.set(m_currentCristal.get_B());
  //////////////
  m_UB.set(0.1, 0.,  0.,
           0., -0.1, 0.,
           0.,  0., -0.1);
  m_UB.set(4.07999046 / 6.283185307179, 0.,  0.,
           0., -4.07999046 / 6.283185307179, 0.,
           0.,  0., -4.07999046 / 6.283185307179);
  //////////////
}

angleConfiguration* eulerianDiffractometer4C::computeAngles(
  double h, double k, double l)
  throw (HKLException)
{
  try
  {
    angleConfiguration* ac = m_currentMode->computeAngles(
      h, k, l, m_UB, m_currentSource.getWaveLength());
    setAngleConfiguration(ac);
    // In the future we intend to move the matrices and the
    // current angles in the field m_currentMode because 
    // their values are mode-depending. At the moment the
    // 6-cercles algorithm has not been implemented so we
    // cannot change the architecture but plan to move this
    // peace of code in a near-by future.
    double small_omega = 
      ((eulerian_angleConfiguration4C*)ac)->getOmega();
    double big_omega = 
      ((eulerian_angleConfiguration4C*)ac)->get2Theta()/2.;
    ((eulerian_angleConfiguration4C*)ac)->setOmega(
      small_omega + big_omega);
    return ac;
  }
  catch (const HKLException&)
  {
    throw;
  }
}

eulerianDiffractometer4C::~eulerianDiffractometer4C()
{
}

smatrix eulerianDiffractometer4C::computeU(
  angleConfiguration* ac1, int h1, int k1, int l1,
  angleConfiguration* ac2, int h2, int k2, int l2)
{
  smatrix R;
  return R;
}

smatrix eulerianDiffractometer4C::computeU(
    reflection& r1, reflection& r2)
{
  // Compute h1c and h2c from equation (17) h1c = B.h1
  // William R. Busing and Henri A. Levy "Angle calculation
  // for 3- and 4- Circle X-ray and Neutron Diffractometer"
  // (1967) Acta Cryst., 22, 457-464.
  // h1c = B.h1
  // h2c = B.h2
  // h1phi = U.h1c
  // h2phi = U.h2c
  // u1phi = R1t.(1,0,0)
  // u2phi = R2t.(1,0,0)
  // h1phi // u1phi
  // h2phi // P(u1phi,u2phi)
  int h1 = r1.get_h();
  int h2 = r2.get_h();
  int k1 = r1.get_k();
  int k2 = r2.get_k();
  int l1 = r1.get_l();
  int l2 = r2.get_l();
  svector h1c(h1, k1, l1);
  svector h2c(h2, k2, l2);
  h1c.multiplyOnTheLeft(m_currentCristal.get_B());
  h2c.multiplyOnTheLeft(m_currentCristal.get_B());

  // Compute matrix Tc from h1c and h2c.
  smatrix Tc;
  h1c.axisSystem(h2c, Tc);
  Tc.transpose();

  svector result;
  h1c.vectorialProduct(h2c,result);
  if (result.norminf() < 
    mathematicalConstants::getEpsilon1())
  {
    // We call this function from the constructor which
    // does not seem to deal with exceptions on this
    // platform and compiler (Visual C++ 6.0 on XP) so
    // if the reflections are parallel set the matrix to 0
    // and catch an exception later in computeAngles().
    m_U.set(
      0., 0., 0.,
      0., 0., 0.,
      0., 0., 0.);
    m_UB.set(m_U);
    return m_U;
  }

  // Compute u1phi and u2phi from the rotation matrices.
  smatrix R1 = computeR(r1.getAngleConfiguration());
  R1.transpose();
  svector u1phi(1., 0., 0.);
  u1phi.multiplyOnTheLeft(R1);
  smatrix R2 = computeR(r2.getAngleConfiguration());
  R2.transpose();
  svector u2phi(1., 0., 0.);
  u2phi.multiplyOnTheLeft(R2);

  // Compute Tphi.
  smatrix Tphi;
  u1phi.axisSystem(u2phi, Tphi);

  u1phi.vectorialProduct(u2phi,result);
  if (result.norminf() < 
    mathematicalConstants::getEpsilon1())
  {
    m_U.set(
      0., 0., 0.,
      0., 0., 0.,
      0., 0., 0.);
    m_UB.set(m_U);
    return m_U;
  }

  // Compute U from equation (27).
  Tphi.multiplyOnTheRight(Tc);
  m_U.set(Tphi);

  // Compute UB.
  m_UB.set(m_U);
  m_UB.multiplyOnTheRight(m_currentCristal.get_B());

  return m_U;
}

// In this function we set the current angle configuration
// and the rotation matrices according to their respective
// rotation axes.
void eulerianDiffractometer4C::setAngleConfiguration(
  angleConfiguration* ac)
{
  double omega     =
    ((eulerian_angleConfiguration4C*)ac)->getOmega();
  double chi       =
    ((eulerian_angleConfiguration4C*)ac)->getChi();
  double phi       =
    ((eulerian_angleConfiguration4C*)ac)->getPhi();
  double two_theta =
    ((eulerian_angleConfiguration4C*)ac)->get2Theta();
  double cos_omega     = cos(omega);
  double cos_chi       = cos(chi);
  double cos_phi       = cos(phi);
  double cos_two_theta = cos(two_theta);
  double sin_omega     = sin(omega);
  double sin_chi       = sin(chi);
  double sin_phi       = sin(phi);
  double sin_two_theta = sin(two_theta);

  // Matrix Omega
  //  | cos_omega   sin_omega 0. |
  //  | -sin_omega  cos_omega 0. |
  //  |      0.        0.     1. |
  m_OMEGA.set(
    cos_omega, sin_omega, 0.,
    -sin_omega,  cos_omega, 0.,
    0., 0., 1.);

  // Matrix Chi
  //  |  cos_chi  0.  sin_chi |
  //  |   0.      1.    0.    |
  //  | -sin_chi  0.  cos_chi |
  m_CHI.set(
    cos_chi, 0., sin_chi,
    0.,  1., 0.,
    -sin_chi, 0., cos_chi);

  // Matrix Phi
  //  |  cos_phi   sin_phi   0. |
  //  | -sin_phi   cos_phi   0. |
  //  |      0.        0.    1. |
  m_PHI.set(
    cos_phi, sin_phi, 0.,
    -sin_phi,  cos_phi, 0.,
    0., 0., 1.);

  // Matrix 2Theta
  //  | cos_two_theta   sin_two_theta 0. |
  //  | -sin_two_theta  cos_two_theta 0. |
  //  |      0.                0.     1. |
  m_2THETA.set(
    cos_two_theta, sin_two_theta, 0.,
    -sin_two_theta,  cos_two_theta, 0.,
    0., 0., 1.);
}

smatrix eulerianDiffractometer4C::computeR()
{
  smatrix R(m_OMEGA);
  R.multiplyOnTheRight(m_CHI);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

smatrix eulerianDiffractometer4C::computeR(
  angleConfiguration* ac)
{
  setAngleConfiguration(ac);
  smatrix R(m_OMEGA);
  R.multiplyOnTheRight(m_CHI);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

void eulerianDiffractometer4C::printOnScreen() const
{
  diffractometer::printOnScreen();
  cout << endl << "CLASS eulerianDiffractometer4C";
  //m_currentSource.printOnScreen();
  //m_currentCristal.printOnScreen();
}

// Test if the algorithm returns a correct angle 
// configuration with respect to a given (h,k,l).
// Return 0 if OK otherwise return the number of 
// the failing test.
int eulerianDiffractometer4C::test_eulerian4C()
{
  // First of all we create a light source whose
  // significant parameter is the wave length
  // (the first one). Then we create a cristal,
  // from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The
  // matrix U comes from two reflections which
  // are stored in a diffractometer. Calling
  // computeAngles() triggers the main computations.

  /////////////
  // TEST 1 //
  ///////////
  int h, k, l;
  // Creation of a light source.
  source _source1(1.54,2.36,5.68);
  // Creation of a crystal.
  cristal cubic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  double degToRad = 
    mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections 
  // needed to compute U.
  eulerian_angleConfiguration4C* eul4C_1 =
    new eulerian_angleConfiguration4C(
      0.*degToRad, 0., 0., 60.*degToRad);
  reflection r1(eul4C_1, 1, 0, 0, 
    reflection::relevance::Best);
  delete eul4C_1;

  eulerian_angleConfiguration4C* eul4C_2 =
    new eulerian_angleConfiguration4C(
      0.*degToRad, 0., -90.*degToRad, 60.*degToRad);
  reflection r2(eul4C_2, 0, 1, 0, 
    reflection::relevance::Best);
  delete eul4C_2;

  h = 1;
  k = 0;
  l = 0;
  // After setting (h,k,l) creation of a diffractometer.
  eulerianDiffractometer4C diff_4C_8(
    cubic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac8 =
    (eulerian_angleConfiguration4C*)
    diff_4C_8.computeAngles(h,k,l);
  // Check the value of the angles.
  if (fabs(eac8->getOmega() - eac8->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->getChi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->getPhi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->get2Theta() - 1.04719755142099)
    > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  delete eac8;

  /////////////
  // TEST 2 //
  ///////////
  h = 0;
  k = 1;
  l = 0;
  eulerianDiffractometer4C diff_4C_9(
    cubic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac9 =
    (eulerian_angleConfiguration4C*)
    diff_4C_9.computeAngles(h,k,l);
  if (fabs(eac9->getOmega() - eac9->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  if (fabs(eac9->getChi() - 3.14159265358979) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  if (fabs(eac9->getPhi() - 89.9999999882484*degToRad) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  if (fabs(eac9->get2Theta() - 1.04719755142099) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  delete eac9;

  /////////////
  // TEST 3 //
  ///////////
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_10(
    cubic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac10 =
    (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getOmega() - eac10->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->getChi() - 0.615479708670) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->getPhi() + 0.7853981634) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->get2Theta() - 2.09439510239) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  delete eac10;

  /////////////
  // TEST 4 //
  ///////////
  h = 1;
  k = -1;
  l = 1;
  eac10 = (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getOmega() - eac10->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->getChi() + 0.615479708670) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->getPhi() - 0.7853981634) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->get2Theta() - 2.09439510239) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  delete eac10;

  /////////////
  // TEST 5 //
  ///////////
  h = 1;
  k = -1;
  l = 1;
  cristal triclinic_cristal1(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  eulerianDiffractometer4C diff_4C_11(
    triclinic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac11 =
    (eulerian_angleConfiguration4C*)
    diff_4C_11.computeAngles(h,k,l);
  if (fabs(eac11->getOmega() - eac11->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->getChi() + 0.50074641878936) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->getPhi() - 1.1282872259289) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  delete eac11;

  /////////////
  // TEST 6 //
  ///////////
  eulerian_angleConfiguration4C* eul4C_3 =
    new eulerian_angleConfiguration4C(
      (20.6255-11.2515/2.)*degToRad,
      0.,
      -15.*degToRad,
      11.2515*degToRad);
  reflection r3(eul4C_3, 1, 0, 0, 
    reflection::relevance::Best);
  delete eul4C_3;
  eulerian_angleConfiguration4C* eul4C_4 =
    new eulerian_angleConfiguration4C(
      (21.3545-12.7090/2.)*degToRad, 0.,
      -72.616*degToRad, 12.7090*degToRad);
  reflection r4(eul4C_4, 0, 1, 0, 
    reflection::relevance::Best);
  delete eul4C_4;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_3(
    cubic_cristal1, _source1, r3, r4,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac3 =
    (eulerian_angleConfiguration4C*)
    diff_4C_3.computeAngles(h,k,l);
  if (fabs(eac3->getOmega() - eac3->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->getChi() - 0.61547970867039) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->getPhi() + 0.78540252672058) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->get2Theta() - 2.0943951023932) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  delete eac3;

  /////////////
  // TEST 7 //
  ///////////
  h = 1;
  k = -1;
  l = 1;
  eulerianDiffractometer4C diff_4C_4(
    triclinic_cristal1, _source1, r3, r4,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac4 =
    (eulerian_angleConfiguration4C*)
    diff_4C_4.computeAngles(h,k,l);
  if (fabs(eac4->getOmega() - eac4->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->getChi() + 0.50074641878936) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->getPhi() - 1.1282828626057) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  delete eac4;

  /////////////
  // TEST 8 //
  ///////////
  eulerian_angleConfiguration4C* eul4C_5 =
    new eulerian_angleConfiguration4C(
      (-13.9535-48.8585/2.)*degToRad,
      -59.5210*degToRad,
      -12.56*degToRad,
      48.8585*degToRad);
  reflection r5(eul4C_5, -1, 3, 5, 
    reflection::relevance::Best);
  delete eul4C_5;
  eulerian_angleConfiguration4C* eul4C_6 =
    new eulerian_angleConfiguration4C(
      (45.651-42.8625/2.)*degToRad,
      9.6535*degToRad,
      -55.4835*degToRad,
      42.8625*degToRad);
  reflection r6(eul4C_6, 2, 2, -1, 
    reflection::relevance::Best);
  delete eul4C_6;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_5(
    cubic_cristal1, _source1, r5, r6,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac5 =
    (eulerian_angleConfiguration4C*)
    diff_4C_5.computeAngles(h,k,l);
  if (fabs(eac5->getOmega() - eac5->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->getChi() - 0.86804479702510) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->getPhi() + 0.13598637990087) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->get2Theta() - 2.0943951023932) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  delete eac5;

  /////////////
  // TEST 9 //
  ///////////
  eulerian_angleConfiguration4C* eul4C_7 =
    new eulerian_angleConfiguration4C(
      (-13.9535-48.8585/2.)*degToRad,
      -59.5210*degToRad,
      -12.56*degToRad,
      48.8585*degToRad);
  reflection r7(eul4C_7, -1, 3, 5, 
    reflection::relevance::Best);
  delete eul4C_7;
  eulerian_angleConfiguration4C* eul4C_8 =
    new eulerian_angleConfiguration4C(
      (45.651-42.8625/2.)*degToRad,
      9.6535*degToRad,
      -55.4835*degToRad,
      42.8625*degToRad);
  reflection r8(eul4C_8, 2, 2, -1, 
    reflection::relevance::Best);
  delete eul4C_8;
  h = 1;
  k = -1;
  l = 1;
  eulerianDiffractometer4C diff_4C_6(
    triclinic_cristal1, _source1, r7, r8,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* _eac9 =
    (eulerian_angleConfiguration4C*)
    diff_4C_6.computeAngles(h,k,l);
  if (fabs(_eac9->getOmega() - _eac9->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->getChi() + 0.50074572470125) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->getPhi() - 1.1282833085965) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  delete _eac9;

  //////////////
  // TEST 10 //
  ////////////
  cristal triclinic_cristal2(
    89.990 * mathematicalConstants::getPI() / 180.,
    89.963 * mathematicalConstants::getPI() / 180.,
    119.99 * mathematicalConstants::getPI() / 180.,
    18.423, 18.417, 18.457);
    //18.423 / (2 * PI),
    //18.417 / (2 * PI),
    //18.457 / (2 * PI));
  source _source2(0.7093,2.36,5.68);
  eulerian_angleConfiguration4C* eul4C_3_ =
    new eulerian_angleConfiguration4C(
      (2.542-5.044/2.)*degToRad,
      -26.15*degToRad,
      92.925*degToRad,
      5.044*degToRad);
  reflection r3_(eul4C_3_, 2, -2, 0, 
    reflection::relevance::Best);
  delete eul4C_3_;
  eulerian_angleConfiguration4C* eul4C_4_ =
    new eulerian_angleConfiguration4C(
      (2.538-5.095/2.)*degToRad,
      71.19*degToRad,
      -12.37*degToRad,
      5.095*degToRad);
  reflection r4_(eul4C_4_, -2, 0, 0, 
    reflection::relevance::Best);
  delete eul4C_4_;
  h = 10;
  k = -8;
  l = 4;
  eulerianDiffractometer4C diff_4C_10_(
    triclinic_cristal2, _source2, r3_, r4_,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac10_ =
    (eulerian_angleConfiguration4C*)
    diff_4C_10_.computeAngles(h,k,l);
  if (fabs(eac10_->getOmega() - eac10_->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->getChi() - 3.5967851323293) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->getPhi() + 1.0667584081915) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->get2Theta() - 0.43899437027362) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  delete eac10_;


  return 0;
}

// ****************************
// * Kappa 4C diffractometer *
// **************************
kappaDiffractometer4C::kappaDiffractometer4C(
  cristal currentCristal, source currentSource,
  reflection& reflection1, reflection& reflection2,
  mode::diffractometer_mode currentMode) : diffractometer(
    currentCristal, currentSource,
    reflection1, reflection2)
{
  m_currentMode = 0;
  //if (currentMode == mode::diffractometer_mode::bissector)
    //m_currentMode = new kappa_bissectorMode4C();
}

kappaDiffractometer4C::~kappaDiffractometer4C()
{
}

smatrix kappaDiffractometer4C::computeR()
{
  smatrix R(m_OMEGA);
  R.multiplyOnTheRight(m_OPP_ALPHA);
  R.multiplyOnTheRight(m_KAPPA);
  R.multiplyOnTheRight(m_ALPHA);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

smatrix kappaDiffractometer4C::computeR(angleConfiguration* ac)
{
  smatrix R(m_OMEGA);
  R.multiplyOnTheRight(m_OPP_ALPHA);
  R.multiplyOnTheRight(m_KAPPA);
  R.multiplyOnTheRight(m_ALPHA);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

smatrix kappaDiffractometer4C::computeU(reflection& r1, reflection& r2)
{
  smatrix R;
  return R;
}

smatrix kappaDiffractometer4C::computeU(
    angleConfiguration* ac1, int h1, int k1, int l1,
    angleConfiguration* ac2, int h2, int k2, int l2)
{
  smatrix R;
  return R;
}

void kappaDiffractometer4C::printOnScreen() const
{
}

void kappaDiffractometer4C::setAngleConfiguration(
  angleConfiguration* ac)
{
}
