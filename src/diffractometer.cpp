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

#define reflectionArraySize 100

// *********************************
// * Abstract type diffractometer *
// *******************************
diffractometer::diffractometer(
  cristal currentCristal,
  source currentSource,
  reflection& reflection1,
  reflection& reflection2):m_sizeOfArray(reflectionArraySize),
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

// Empty constructor.
diffractometer::diffractometer():m_sizeOfArray(reflectionArraySize),
    m_currentCristal(0.,0.,0.,0.,0.,0.),
    m_numberOfInsertedElements(0),
    m_currentSource(0.,0.,0.)
{
  m_reflectionList = new reflection[m_sizeOfArray];
  m_UB.set(0.,0.,0.,0.,0.,0.,0.,0.,0.);
}

// Constructor designed for testing purposes.
diffractometer::diffractometer(
  cristal currentCristal,
  source currentSource):m_sizeOfArray(reflectionArraySize),
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

double diffractometer::getReflection_h(int i) const
{
  return m_reflectionList[i].get_h();
}

double diffractometer::getReflection_k(int i) const
{
  return m_reflectionList[i].get_k();
}

double diffractometer::getReflection_l(int i) const
{
  return m_reflectionList[i].get_l();
}

void diffractometer::setReflection(
  angleConfiguration* ac,
  double h, double k, double l, reflection::relevance r, int index)
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

void diffractometer::setCrystal(
  double alpha1, double alpha2, double alpha3,
  double a1, double a2, double a3)
{
  m_currentCristal.set(alpha1, alpha2, alpha3, a1, a2, a3);
}

void diffractometer::setCrystal(const cristal& C)
{
  m_currentCristal.set(C);
}

void diffractometer::setWaveLength(double wl)
{
  m_currentSource.setWaveLength(wl);
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
  setMode(currentMode);
  /*
  if (currentMode == mode::diffractometer_mode::bissector)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_bissectorMode4C();
  }
  */
}

// Default constructor.
eulerianDiffractometer4C::eulerianDiffractometer4C() : diffractometer()
{
  m_currentMode = 0;
}

// Constructor designed for testing, U = unit matrix or any other arbitrarily set.
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

// Constructor designed for the 6C diffractometer.
eulerianDiffractometer4C::eulerianDiffractometer4C(
  cristal currentCristal, source currentSource) : diffractometer(
    currentCristal, currentSource)
{
}

// Change the current computational mode.
void eulerianDiffractometer4C::setMode(mode::diffractometer_mode currentMode)
{
  if (currentMode == mode::diffractometer_mode::bissector)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_bissectorMode4C();
  }
}

angleConfiguration* eulerianDiffractometer4C::computeAngles(
  double h, double k, double l)
  throw (HKLException)
{
  try
  {
    if (m_currentMode == 0)
      throw HKLException(
        "m_currentMode is null",
        "The mode has not been set",
        "eulerianDiffractometer4C::computeAngles()");

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

angleConfiguration* eulerianDiffractometer4C::computeAngles_Rafin(
  double h, double k, double l)
  throw (HKLException)
{
  try
  {
    if (m_currentMode == 0)
      throw HKLException(
        "m_currentMode is null",
        "The mode has not been set",
        "eulerianDiffractometer4C::computeAngles()");

    angleConfiguration* ac = m_currentMode->computeAngles_Rafin(
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

void eulerianDiffractometer4C::computeHKL(
  double& h, double& k, double& l, angleConfiguration* ac)
  throw (HKLException)
{
  try
  {
    if (m_currentMode == 0)
      throw HKLException(
        "m_currentMode is null",
        "The mode has not been set",
        "eulerianDiffractometer4C::computeHKL()");

    m_currentMode->computeHKL(h,k,l,m_UB,m_currentSource.getWaveLength(),ac);
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
  angleConfiguration* ac1, double h1, double k1, double l1,
  angleConfiguration* ac2, double h2, double k2, double l2)
{
  reflection r1(ac1, h1, k1, l1, reflection::relevance::Best);
  reflection r2(ac2, h2, k2, l2, reflection::relevance::Best);
  return computeU(r1,r2);
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

int test4_eulerian4C()
{
  // First of all we create a light source whose
  // significant parameter is the wave length
  // (the first one). Then we create a cristal,
  // from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The
  // matrix U comes from two reflections which
  // are stored in a diffractometer. Calling
  // computeAngles() triggers the main computations.


  //////////////
  // TEST 31 //
  ////////////
  int ii = 31;
  int h, k, l;
  double H,K,L;
  // Creation of a light source.
  //source _source1(1.54,2.36,5.68);
  // Creation of a crystal.
  //cristal cubic_cristal1(
  //  1.5707963267948966,1.5707963267948966,1.5707963267948966,
  //  1.54, 1.54, 1.54);
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections 
  // needed to compute U.
  eulerian_angleConfiguration4C* eul4C_1 =
    new eulerian_angleConfiguration4C(
      0.*degToRad, 0., 0., 60.*degToRad);
  //reflection r1(eul4C_1, 1, 0, 0, 
  //  reflection::relevance::Best);

  eulerian_angleConfiguration4C* eul4C_2 =
    new eulerian_angleConfiguration4C(
      0.*degToRad, 0., -90.*degToRad, 60.*degToRad);
  //reflection r2(eul4C_2, 0, 1, 0, 
  //  reflection::relevance::Best);

  h = 1;
  k = 0;
  l = 0;

  // After setting (h,k,l) create  a diffractometer.
  eulerianDiffractometer4C diff_4C_8;
  // Set the crystal inside the diffractometer and
  // compute its matrix B.
  diff_4C_8.setCrystal(
    1.5707963267948966, 1.5707963267948966, 1.5707963267948966,
    1.54, 1.54, 1.54);
  // Set the wave length corresponding to the light source.
  diff_4C_8.setWaveLength(1.54);
  // Decide which mode to work with.
  diff_4C_8.setMode(mode::diffractometer_mode::bissector);
  // Compute the orientation matrix from the two reflections.
  diff_4C_8.computeU(eul4C_1, 1, 0, 0, eul4C_2, 0, 1, 0);
  // Get the angle configuration corresponding to the given (h,k,l).
  eulerian_angleConfiguration4C* eac8 =
    (eulerian_angleConfiguration4C*) diff_4C_8.computeAngles(h,k,l);

  diff_4C_8.computeHKL(H,K,L,eac8);

  // Check the value of the angles.
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    //delete eac8b;
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    //delete eac8b;
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    //delete eac8b;
    delete eac8;
    return ii;
  }
  delete eac8;

  //////////////
  // TEST 32 //
  ////////////
  ii = 32;
  h = 0;
  k = 1;
  l = 0;
  eulerianDiffractometer4C diff_4C_9;
  diff_4C_9.setCrystal(
    1.5707963267948966, 1.5707963267948966, 1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_9.setWaveLength(1.54);
  diff_4C_9.setMode(mode::diffractometer_mode::bissector);
  diff_4C_9.computeU(eul4C_1, 1, 0, 0, eul4C_2, 0, 1, 0);

  eulerian_angleConfiguration4C* eac9 =
    (eulerian_angleConfiguration4C*) diff_4C_9.computeAngles(h,k,l);

  diff_4C_9.computeHKL(H,K,L,eac9);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  delete eac9;

  //////////////
  // TEST 33 //
  ////////////
  ii = 33;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_10;
  diff_4C_10.setCrystal(
    1.5707963267948966, 1.5707963267948966, 1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_10.setWaveLength(1.54);
  diff_4C_10.setMode(mode::diffractometer_mode::bissector);
  diff_4C_10.computeU(eul4C_1, 1, 0, 0, eul4C_2, 0, 1, 0);

  eulerian_angleConfiguration4C* eac10 =
    (eulerian_angleConfiguration4C*) diff_4C_10.computeAngles(h,k,l);

  diff_4C_10.computeHKL(H,K,L,eac10);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  delete eac10;

  //////////////
  // TEST 34 //
  ////////////
  ii = 34;
  h = 1;
  k = -1;
  l = 1;

  eac10 = (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);

  diff_4C_10.computeHKL(H,K,L,eac10);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return ii;
  }
  delete eac10;

  //////////////
  // TEST 35 //
  ////////////
  ii = 35;
  h = 1;
  k = -1;
  l = 1;
  eulerianDiffractometer4C diff_4C_11;
  diff_4C_11.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_11.setWaveLength(1.54);
  diff_4C_11.setMode(mode::diffractometer_mode::bissector);
  diff_4C_11.computeU(eul4C_1, 1, 0, 0, eul4C_2, 0, 1, 0);

  eulerian_angleConfiguration4C* eac11 =
    (eulerian_angleConfiguration4C*) diff_4C_11.computeAngles(h,k,l);

  diff_4C_11.computeHKL(H,K,L,eac11);

  delete eul4C_1;
  delete eul4C_2;

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  delete eac11;

  //////////////
  // TEST 36 //
  ////////////
  eulerian_angleConfiguration4C* eul4C_3 =
    new eulerian_angleConfiguration4C(
      (20.6255-11.2515/2.)*degToRad, 0.,
      -15.*degToRad, 11.2515*degToRad);
  eulerian_angleConfiguration4C* eul4C_4 =
    new eulerian_angleConfiguration4C(
      (21.3545-12.7090/2.)*degToRad, 0.,
      -72.616*degToRad, 12.7090*degToRad);
  reflection r3(eul4C_3, 1, 0, 0, 
    reflection::relevance::Best);
  reflection r4(eul4C_4, 0, 1, 0, 
    reflection::relevance::Best);

  ii = 36;
  h = 1;
  k = 1;
  l = -1;

  eulerianDiffractometer4C diff_4C_3;
  diff_4C_3.setCrystal(
    1.5707963267948966, 1.5707963267948966, 1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_3.setWaveLength(1.54);
  diff_4C_3.setMode(mode::diffractometer_mode::bissector);
  diff_4C_3.computeU(eul4C_3, 1, 0, 0, eul4C_4, 0, 1, 0);

  eulerian_angleConfiguration4C* eac3 =
    (eulerian_angleConfiguration4C*) diff_4C_3.computeAngles(h,k,l);

  diff_4C_3.computeHKL(H,K,L,eac3);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return ii;
  }
  delete eac3;

  //////////////
  // TEST 37 //
  ////////////
  ii = 37;
  h = 1;
  k = -1;
  l = 1;

  eulerianDiffractometer4C diff_4C_4;
  diff_4C_4.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_4.setWaveLength(1.54);
  diff_4C_4.setMode(mode::diffractometer_mode::bissector);
  diff_4C_4.computeU(eul4C_3, 1, 0, 0, eul4C_4, 0, 1, 0);

  eulerian_angleConfiguration4C* eac4 =
    (eulerian_angleConfiguration4C*) diff_4C_4.computeAngles(h,k,l);

  diff_4C_4.computeHKL(H,K,L,eac4);

  delete eul4C_3;
  delete eul4C_4;

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return ii;
  }
  delete eac4;

  //////////////
  // TEST 38 //
  ////////////
  eulerian_angleConfiguration4C* eul4C_5 =
    new eulerian_angleConfiguration4C(
      (-13.9535-48.8585/2.)*degToRad, -59.5210*degToRad,
      -12.56*degToRad, 48.8585*degToRad);
  eulerian_angleConfiguration4C* eul4C_6 =
    new eulerian_angleConfiguration4C(
      (45.651-42.8625/2.)*degToRad, 9.6535*degToRad,
      -55.4835*degToRad, 42.8625*degToRad);
  reflection r5(eul4C_5, -1, 3, 5, 
    reflection::relevance::Best);
  reflection r6(eul4C_6, 2, 2, -1, 
    reflection::relevance::Best);

  ii = 38;
  h = 1;
  k = 1;
  l = -1;

  eulerianDiffractometer4C diff_4C_5;
  diff_4C_5.setCrystal(
    1.5707963267948966, 1.5707963267948966, 1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_5.setWaveLength(1.54);
  diff_4C_5.setMode(mode::diffractometer_mode::bissector);
  diff_4C_5.computeU(eul4C_5, -1, 3, 5, eul4C_6, 2, 2, -1);
  delete eul4C_5;
  delete eul4C_6;

  eulerian_angleConfiguration4C* eac5 =
    (eulerian_angleConfiguration4C*) diff_4C_5.computeAngles(h,k,l);

  diff_4C_5.computeHKL(H,K,L,eac5);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return ii;
  }
  delete eac5;

  //////////////
  // TEST 39 //
  ////////////
  eulerian_angleConfiguration4C* eul4C_7 =
    new eulerian_angleConfiguration4C(
      (-13.9535-48.8585/2.)*degToRad, -59.5210*degToRad,
      -12.56*degToRad, 48.8585*degToRad);
  eulerian_angleConfiguration4C* eul4C_8 =
    new eulerian_angleConfiguration4C(
      (45.651-42.8625/2.)*degToRad, 9.6535*degToRad,
      -55.4835*degToRad, 42.8625*degToRad);
  reflection r7(eul4C_7, -1, 3, 5, 
    reflection::relevance::Best);
  reflection r8(eul4C_8, 2, 2, -1, 
    reflection::relevance::Best);

  ii = 39;
  h = 1;
  k = -1;
  l = 1;

  eulerianDiffractometer4C diff_4C_6;
  diff_4C_6.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_6.setWaveLength(1.54);
  diff_4C_6.setMode(mode::diffractometer_mode::bissector);
  diff_4C_6.computeU(eul4C_7, -1, 3, 5, eul4C_8, 2, 2, -1);
  delete eul4C_7;
  delete eul4C_8;

  eulerian_angleConfiguration4C* _eac9 =
    (eulerian_angleConfiguration4C*) diff_4C_6.computeAngles(h,k,l);

  diff_4C_6.computeHKL(H,K,L,_eac9);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return ii;
  }
  delete _eac9;

  //////////////
  // TEST 40 //
  ////////////
  eulerian_angleConfiguration4C* eul4C_3_ =
    new eulerian_angleConfiguration4C(
      (2.542-5.044/2.)*degToRad, -26.15*degToRad,
      92.925*degToRad, 5.044*degToRad);
  eulerian_angleConfiguration4C* eul4C_4_ =
    new eulerian_angleConfiguration4C(
      (2.538-5.095/2.)*degToRad, 71.19*degToRad,
      -12.37*degToRad, 5.095*degToRad);
  reflection r3_(eul4C_3_, 2, -2, 0, 
    reflection::relevance::Best);
  reflection r4_(eul4C_4_, -2, 0, 0, 
    reflection::relevance::Best);

  ii =40;
  h = 5;
  k = -4;
  l = 2;

  eulerianDiffractometer4C diff_4C_10_;
  diff_4C_10_.setCrystal(
    89.990 * mathematicalConstants::getPI() / 180.,
    89.963 * mathematicalConstants::getPI() / 180.,
    119.99 * mathematicalConstants::getPI() / 180.,
    18.423, 18.417, 18.457);
  diff_4C_10_.setWaveLength(0.7093);
  diff_4C_10_.setMode(mode::diffractometer_mode::bissector);
  diff_4C_10_.computeU(eul4C_3_, 2, -2, 0, eul4C_4_, -2, 0, 0);
  delete eul4C_4_;
  delete eul4C_3_;

  eulerian_angleConfiguration4C* eac10_ =
    (eulerian_angleConfiguration4C*) diff_4C_10_.computeAngles(h,k,l);

  diff_4C_10_.computeHKL(H,K,L,eac10_);

  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return ii;
  }
  delete eac10_;

  return 0;
}

int test3_eulerian4C()
{
  // First of all we create a light source whose
  // significant parameter is the wave length
  // (the first one). Then we create a cristal,
  // from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The
  // matrix U comes from two reflections which
  // are stored in a diffractometer. Calling
  // computeAngles() triggers the main computations.


  //////////////
  // TEST 21 //
  ////////////
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
  eulerian_angleConfiguration4C* eac8b =
    (eulerian_angleConfiguration4C*)
    diff_4C_8.computeAngles_Rafin(h,k,l);
  // Check the value of the angles.
  if (fabs(eac8->getOmega() - eac8b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8b;
    delete eac8;
    return 21;
  }
  if (fabs(eac8->getChi() - eac8b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8b;
    delete eac8;
    return 21;
  }
  if (fabs(eac8->getPhi() - eac8b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8b;
    delete eac8;
    return 21;
  }
  if (fabs(eac8->get2Theta() - eac8b->get2Theta())
    > mathematicalConstants::getEpsilon0())
  {
    delete eac8b;
    delete eac8;
    return 21;
  }
  delete eac8b;
  delete eac8;

  //////////////
  // TEST 22 //
  ////////////
  h = 0;
  k = 1;
  l = 0;
  eulerianDiffractometer4C diff_4C_9(
    cubic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);

  eulerian_angleConfiguration4C* eac9 =
    (eulerian_angleConfiguration4C*)
    diff_4C_9.computeAngles(h,k,l);

  eulerian_angleConfiguration4C* eac9b =
    (eulerian_angleConfiguration4C*)
    diff_4C_9.computeAngles_Rafin(h,k,l);

  if (fabs(eac9->getOmega() - eac9b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9b;
    delete eac9;
    return 22;
  }
  if (fabs(eac9->getChi() - eac9b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9b;
    delete eac9;
    return 22;
  }
  if (fabs(eac9->getPhi() - eac9b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9b;
    delete eac9;
    return 22;
  }
  if (fabs(eac9->get2Theta() - eac9b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9b;
    delete eac9;
    return 22;
  }
  delete eac9b;
  delete eac9;

  //////////////
  // TEST 23 //
  ////////////
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_10(
    cubic_cristal1, _source1, r1, r2,
    mode::diffractometer_mode::bissector);

  eulerian_angleConfiguration4C* eac10 =
    (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);

  eulerian_angleConfiguration4C* eac10b =
    (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles_Rafin(h,k,l);

  if (fabs(eac10->getOmega() - eac10b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 23;
  }
  if (fabs(eac10->getChi() - eac10b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 23;
  }
  if (fabs(eac10->getPhi() - eac10b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 23;
  }
  if (fabs(eac10->get2Theta() - eac10b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 23;
  }
  delete eac10b;
  delete eac10;

  //////////////
  // TEST 24 //
  ////////////
  h = 1;
  k = -1;
  l = 1;

  eac10 = (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);

  eac10b = (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles_Rafin(h,k,l);

  if (fabs(eac10->getOmega() - eac10b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 24;
  }
  if (fabs(eac10->getChi() - eac10b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 24;
  }
  if (fabs(eac10->getPhi() - eac10b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 24;
  }
  if (fabs(eac10->get2Theta() - eac10b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10b;
    delete eac10;
    return 24;
  }
  delete eac10b;
  delete eac10;

  //////////////
  // TEST 25 //
  ////////////
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

  eulerian_angleConfiguration4C* eac11b =
    (eulerian_angleConfiguration4C*)
    diff_4C_11.computeAngles_Rafin(h,k,l);

  if (fabs(eac11->getOmega() - eac11b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11b;
    delete eac11;
    return 25;
  }
  if (fabs(eac11->getChi() - eac11b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11b;
    delete eac11;
    return 25;
  }
  if (fabs(eac11->getPhi() - eac11b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11b;
    delete eac11;
    return 25;
  }
  if (fabs(eac11->get2Theta() - eac11b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11b;
    delete eac11;
    return 25;
  }
  delete eac11b;
  delete eac11;

  //////////////
  // TEST 26 //
  ////////////
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

  eulerian_angleConfiguration4C* eac3b =
    (eulerian_angleConfiguration4C*)
    diff_4C_3.computeAngles_Rafin(h,k,l);

  if (fabs(eac3->getOmega() - eac3b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3b;
    delete eac3;
    return 26;
  }
  if (fabs(eac3->getChi() - eac3b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3b;
    delete eac3;
    return 26;
  }
  if (fabs(eac3->getPhi() - eac3b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3b;
    delete eac3;
    return 26;
  }
  if (fabs(eac3->get2Theta() - eac3b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3b;
    delete eac3;
    return 26;
  }
  delete eac3b;
  delete eac3;

  //////////////
  // TEST 27 //
  ////////////
  h = 1;
  k = -1;
  l = 1;

  eulerianDiffractometer4C diff_4C_4(
    triclinic_cristal1, _source1, r3, r4,
    mode::diffractometer_mode::bissector);

  eulerian_angleConfiguration4C* eac4 =
    (eulerian_angleConfiguration4C*)
    diff_4C_4.computeAngles(h,k,l);

  eulerian_angleConfiguration4C* eac4b =
    (eulerian_angleConfiguration4C*)
    diff_4C_4.computeAngles_Rafin(h,k,l);

  if (fabs(eac4->getOmega() - eac4b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4b;
    delete eac4;
    return 27;
  }
  if (fabs(eac4->getChi() - eac4b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4b;
    delete eac4;
    return 27;
  }
  if (fabs(eac4->getPhi() - eac4b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4b;
    delete eac4;
    return 27;
  }
  if (fabs(eac4->get2Theta() - eac4b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4b;
    delete eac4;
    return 27;
  }
  delete eac4b;
  delete eac4;

  //////////////
  // TEST 28 //
  ////////////
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

  eulerian_angleConfiguration4C* eac5b =
    (eulerian_angleConfiguration4C*)
    diff_4C_5.computeAngles_Rafin(h,k,l);

  if (fabs(eac5->getOmega() - eac5b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5b;
    delete eac5;
    return 28;
  }
  if (fabs(eac5->getChi() - eac5b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5b;
    delete eac5;
    return 28;
  }
  if (fabs(eac5->getPhi() - eac5b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5b;
    delete eac5;
    return 28;
  }
  if (fabs(eac5->get2Theta() - eac5b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5b;
    delete eac5;
    return 28;
  }
  delete eac5b;
  delete eac5;

  //////////////
  // TEST 29 //
  ////////////
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

  eulerian_angleConfiguration4C* _eac9b =
    (eulerian_angleConfiguration4C*)
    diff_4C_6.computeAngles_Rafin(h,k,l);

  if (fabs(_eac9->getOmega() - _eac9b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9b;
    delete _eac9;
    return 29;
  }
  if (fabs(_eac9->getChi() - _eac9b->getChi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9b;
    delete _eac9;
    return 29;
  }
  if (fabs(_eac9->getPhi() - _eac9b->getPhi()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9b;
    delete _eac9;
    return 29;
  }
  if (fabs(_eac9->get2Theta() - _eac9b->get2Theta()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9b;
    delete _eac9;
    return 29;
  }
  delete _eac9b;
  delete _eac9;

  //////////////
  // TEST 30 //
  ////////////
  /*
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

  h = 5;
  k = -4;
  l = 2;

  eulerianDiffractometer4C diff_4C_10_(
    triclinic_cristal2, _source2, r3_, r4_,
    mode::diffractometer_mode::bissector);

  eulerian_angleConfiguration4C* eac10_ =
    (eulerian_angleConfiguration4C*)
    diff_4C_10_.computeAngles(h,k,l);

  eulerian_angleConfiguration4C* eac10_b =
    (eulerian_angleConfiguration4C*)
    diff_4C_10_.computeAngles_Rafin(h,k,l);

  if (fabs(eac10_->getOmega() - eac10_b->getOmega()) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_b;
    delete eac10_;
    return 30;
  }
  if (fabs(eac10_->getChi() - eac10_b->getChi()) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_b;
    delete eac10_;
    return 30;
  }
  if (fabs(eac10_->getPhi() - eac10_b->getPhi()) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_b;
    delete eac10_;
    return 30;
  }
  if (fabs(eac10_->get2Theta() - eac10_b->get2Theta()) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_b;
    delete eac10_;
    return 30;
  }
  delete eac10_b;
  delete eac10_;
  */

  return 0;
}

int test1_eulerian4C()
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
  //if (fabs(eac9->getChi() - 3.14159265358979) > 
  if (fabs(eac9->getChi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  //if (fabs(eac9->getPhi() - 89.9999999882484*degToRad) > 
  if (fabs(eac9->getPhi() + 89.9999999882484*degToRad) > 
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

int test2_eulerian4C()
{
  // First of all we create a light source whose
  // significant parameter is the wave length
  // (the first one). Then we create a cristal,
  // from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The
  // matrix U comes from two reflections which
  // are stored in a diffractometer. Calling
  // computeAngles() triggers the main computations.
  int counter;

  //////////////
  // TEST 11 //
  ////////////
  counter = 11;
  int h, k, l;
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
  // After setting (h,k,l) create  a diffractometer.
  eulerianDiffractometer4C diff_4C_8;
  // Set the crystal inside the diffractometer and
  // compute its matrix B.
  diff_4C_8.setCrystal(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  // Set the wave length corresponding to the light source.
  diff_4C_8.setWaveLength(1.54);
  // Decide which mode to work with.
  diff_4C_8.setMode(mode::diffractometer_mode::bissector);
  // Compute the orientation matrix from the two reflections.
  diff_4C_8.computeU(r1,r2);
  // Get the angle configuration corresponding to the 
  // given (h,k,l).
  eulerian_angleConfiguration4C* eac8 =
    (eulerian_angleConfiguration4C*)
    diff_4C_8.computeAngles(h,k,l);
  // Check the value of the angles.
  if (fabs(eac8->getOmega() - eac8->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return counter;
  }
  if (fabs(eac8->getChi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return counter;
  }
  if (fabs(eac8->getPhi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return counter;
  }
  if (fabs(eac8->get2Theta() - 1.04719755142099)
    > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return counter;
  }
  delete eac8;

  //////////////
  // TEST 12 //
  ////////////
  counter = 12;
  h = 0;
  k = 1;
  l = 0;
  eulerianDiffractometer4C diff_4C_9;
  diff_4C_9.setCrystal(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_9.setWaveLength(1.54);
  diff_4C_9.computeU(r1,r2);
  diff_4C_9.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac9 =
    (eulerian_angleConfiguration4C*)
    diff_4C_9.computeAngles(h,k,l);
  if (fabs(eac9->getOmega() - eac9->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return counter;
  }
  //if (fabs(eac9->getChi() - 3.14159265358979) > 
  if (fabs(eac9->getChi() - 0.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return counter;
  }
  //if (fabs(eac9->getPhi() - 89.9999999882484*degToRad) > 
  if (fabs(eac9->getPhi() + 89.9999999882484*degToRad) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return counter;
  }
  if (fabs(eac9->get2Theta() - 1.04719755142099) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return counter;
  }
  delete eac9;

  //////////////
  // TEST 13 //
  ////////////
  counter = 13;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer4C diff_4C_10;
  diff_4C_10.setCrystal(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_10.setWaveLength(1.54);
  diff_4C_10.computeU(r1,r2);
  diff_4C_10.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac10 =
    (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getOmega() - eac10->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->getChi() - 0.615479708670) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->getPhi() + 0.7853981634) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->get2Theta() - 2.09439510239) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  delete eac10;

  //////////////
  // TEST 14 //
  ////////////
  counter = 14;
  h = 1;
  k = -1;
  l = 1;
  eac10 = (eulerian_angleConfiguration4C*)
    diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getOmega() - eac10->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->getChi() + 0.615479708670) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->getPhi() - 0.7853981634) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  if (fabs(eac10->get2Theta() - 2.09439510239) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return counter;
  }
  delete eac10;

  //////////////
  // TEST 15 //
  ////////////
  counter = 15;
  h = 1;
  k = -1;
  l = 1;
  cristal triclinic_cristal1(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  eulerianDiffractometer4C diff_4C_11;
  diff_4C_11.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_11.setWaveLength(1.54);
  diff_4C_11.computeU(r1,r2);
  diff_4C_11.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac11 =
    (eulerian_angleConfiguration4C*)
    diff_4C_11.computeAngles(h,k,l);
  if (fabs(eac11->getOmega() - eac11->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return counter;
  }
  if (fabs(eac11->getChi() + 0.50074641878936) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return counter;
  }
  if (fabs(eac11->getPhi() - 1.1282872259289) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return counter;
  }
  if (fabs(eac11->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return counter;
  }
  delete eac11;

  //////////////
  // TEST 16 //
  ////////////
  counter = 16;
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
  eulerianDiffractometer4C diff_4C_3;
  diff_4C_3.setCrystal(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_3.setWaveLength(1.54);
  diff_4C_3.computeU(r3,r4);
  diff_4C_3.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac3 =
    (eulerian_angleConfiguration4C*)
    diff_4C_3.computeAngles(h,k,l);
  if (fabs(eac3->getOmega() - eac3->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return counter;
  }
  if (fabs(eac3->getChi() - 0.61547970867039) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return counter;
  }
  if (fabs(eac3->getPhi() + 0.78540252672058) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return counter;
  }
  if (fabs(eac3->get2Theta() - 2.0943951023932) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return counter;
  }
  delete eac3;

  //////////////
  // TEST 17 //
  ////////////
  counter = 17;
  h = 1;
  k = -1;
  l = 1;
  eulerianDiffractometer4C diff_4C_4;
  diff_4C_4.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_4.setWaveLength(1.54);
  diff_4C_4.computeU(r3,r4);
  diff_4C_4.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac4 =
    (eulerian_angleConfiguration4C*)
    diff_4C_4.computeAngles(h,k,l);
  if (fabs(eac4->getOmega() - eac4->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return counter;
  }
  if (fabs(eac4->getChi() + 0.50074641878936) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return counter;
  }
  if (fabs(eac4->getPhi() - 1.1282828626057) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return counter;
  }
  if (fabs(eac4->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return counter;
  }
  delete eac4;

  //////////////
  // TEST 18 //
  ////////////
  counter = 18;
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
  eulerianDiffractometer4C diff_4C_5;
  diff_4C_5.setCrystal(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  diff_4C_5.setWaveLength(1.54);
  diff_4C_5.computeU(r5,r6);
  diff_4C_5.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac5 =
    (eulerian_angleConfiguration4C*)
    diff_4C_5.computeAngles(h,k,l);
  if (fabs(eac5->getOmega() - eac5->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return counter;
  }
  if (fabs(eac5->getChi() - 0.86804479702510) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return counter;
  }
  if (fabs(eac5->getPhi() + 0.13598637990087) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return counter;
  }
  if (fabs(eac5->get2Theta() - 2.0943951023932) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return counter;
  }
  delete eac5;

  //////////////
  // TEST 19 //
  ////////////
  counter = 19;
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
  eulerianDiffractometer4C diff_4C_6;
  diff_4C_6.setCrystal(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  diff_4C_6.setWaveLength(1.54);
  diff_4C_6.computeU(r7,r8);
  diff_4C_6.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* _eac9 =
    (eulerian_angleConfiguration4C*)
    diff_4C_6.computeAngles(h,k,l);
  if (fabs(_eac9->getOmega() - _eac9->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return counter;
  }
  if (fabs(_eac9->getChi() + 0.50074572470125) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return counter;
  }
  if (fabs(_eac9->getPhi() - 1.1282833085965) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return counter;
  }
  if (fabs(_eac9->get2Theta() - 0.23331517265342) > 
    mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return counter;
  }
  delete _eac9;

  //////////////
  // TEST 20 //
  ////////////
  counter = 20;
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
  eulerianDiffractometer4C diff_4C_10_;
  diff_4C_10_.setCrystal(
    89.990 * mathematicalConstants::getPI() / 180.,
    89.963 * mathematicalConstants::getPI() / 180.,
    119.99 * mathematicalConstants::getPI() / 180.,
    18.423, 18.417, 18.457);
  diff_4C_10_.setWaveLength(0.7093);
  diff_4C_10_.computeU(r3_,r4_);
  diff_4C_10_.setMode(mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac10_ =
    (eulerian_angleConfiguration4C*)
    diff_4C_10_.computeAngles(h,k,l);
  if (fabs(eac10_->getOmega() - eac10_->get2Theta()/2.) > 
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return counter;
  }
  if (fabs(eac10_->getChi() - 3.5967851323293) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return counter;
  }
  if (fabs(eac10_->getPhi() + 1.0667584081915) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return counter;
  }
  if (fabs(eac10_->get2Theta() - 0.43899437027362) >
    mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return counter;
  }
  delete eac10_;

  return 0;
}

// Test if the algorithm returns a correct angle 
// configuration with respect to a given (h,k,l).
// Return 0 if OK otherwise return the number of 
// the failing test.
int eulerianDiffractometer4C::test_eulerian4C()
{
  // Test angles with full constructors.
  int a = test1_eulerian4C();
  if (0 != a)
    return a;

  // Test angle values with differed settings.
  int b = test2_eulerian4C();
  if (0 != b)
    return b;

  // Test computations with Rafin algorithm.
  int c = test3_eulerian4C();
  if (0 != c)
    return c;

  // Tests angles from (h,k,l) and then (h,k,l) from these angles.
  int d = test4_eulerian4C();
  return d;
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
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2)
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
