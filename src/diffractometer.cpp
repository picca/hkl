// Class diffractometer to drive experiments. Reference : 
// William R. Busing and Henri A. Levy "Angle calculation 
// for 3- and 4- Circle X-ray and Neutron Diffractometer"
// (1967) Acta Cryst., 22, 457-464.
#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "reflection.h"
#include "angleconfig.h"
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
    m_currentMode = new eulerian_bissectorMode4C();
}

eulerianDiffractometer4C::eulerianDiffractometer4C(
  cristal currentCristal, source currentSource,
  mode::diffractometer_mode currentMode) : diffractometer(
    currentCristal, currentSource)
{
  m_currentMode = 0;
  if (currentMode == mode::diffractometer_mode::bissector)
    m_currentMode = new eulerian_bissectorMode4C();

  // Compute UB fro U = unity.
  m_UB.set(m_currentCristal.get_B());
}

angleConfiguration* eulerianDiffractometer4C::computeAngles(
  int h, int k, int l)
{
  angleConfiguration* ac = m_currentMode->computeAngles(
    h, k, l, m_UB, m_currentSource.getWaveLength());
  setAngleConfiguration(ac);

  return ac;
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
