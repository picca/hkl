// Class diffractometer to drive experiments. Reference : 
// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "constants.h"
#include "reflection.h"
#include "angleconfig.h"
#include "HKLException.h"
#include "diffractometer.h"
#include <iostream>
#include <math.h>


// *******************************
// * Eulerian 6C diffractometer *
// *****************************
eulerianDiffractometer6C::eulerianDiffractometer6C(
  cristal currentCristal, source currentSource, reflection& reflection1, reflection& reflection2,
  mode::diffractometer_mode currentMode) : eulerianDiffractometer4C(currentCristal, currentSource)
{
  // Compute UB matrix.
  computeU(reflection1, reflection2);

  m_currentMode = 0;
  setMode(currentMode);
}

// Default constructor.
eulerianDiffractometer6C::eulerianDiffractometer6C() : eulerianDiffractometer4C()
{
  m_currentMode = 0;
}

// Destructor.
eulerianDiffractometer6C::~eulerianDiffractometer6C()
{
}

// Change the current computational mode.
void eulerianDiffractometer6C::setMode(mode::diffractometer_mode currentMode)
{
  if (currentMode == mode::horizontal4CBissector6C)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_horizontal4CBissectorMode6C();
  }
  else if (currentMode == mode::vertical4CBissector6C)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_vertical4CBissectorMode6C();
  }
  else if (currentMode == mode::lifting3CDetector6C)
  {
    if (m_currentMode != 0)
      delete m_currentMode;
    m_currentMode = new eulerian_lifting3CDetectorMode6C();
  }
}

angleConfiguration* eulerianDiffractometer6C::computeAngles(double h, double k, double l)
  throw (HKLException)
{
  try
  {
    if (m_currentMode == 0)
      throw HKLException(
        "m_currentMode is null",
        "The mode has not been set",
        "eulerianDiffractometer6C::computeAngles()");

    angleConfiguration* ac = m_currentMode->computeAngles(h, k, l, m_UB, m_currentSource.getWaveLength());
    setAngleConfiguration(ac);
    return ac;
  }
  catch (const HKLException&)
  {
    throw;
  }
}

void eulerianDiffractometer6C::computeHKL(double& h, double& k, double& l, angleConfiguration* ac)
  throw (HKLException)
{
  try
  {
    if (m_currentMode == 0)
      throw HKLException(
        "m_currentMode is null",
        "The mode has not been set",
        "eulerianDiffractometer6C::computeHKL()");

    m_currentMode->computeHKL(h,k,l,m_UB,m_currentSource.getWaveLength(),ac);
  }
  catch (const HKLException&)
  {
    throw;
  }
}

smatrix eulerianDiffractometer6C::computeU(
  angleConfiguration* ac1, double h1, double k1, double l1,
  angleConfiguration* ac2, double h2, double k2, double l2)
{
  reflection r1(ac1, h1, k1, l1, reflection::Best);
  reflection r2(ac2, h2, k2, l2, reflection::Best);
  return computeU(r1,r2);
}

smatrix eulerianDiffractometer6C::computeU(reflection& r1, reflection& r2)
{
  // The scattering vector is : k(sin(delta), cos(delta).cos(nu)-1, cos(delta).sin(nu))
  // where k = tau/lambda
  // h1c = B.h1
  // h2c = B.h2
  // h1phi = U.h1c
  // h2phi = U.h2c
  // h3phi = h1phi * h2phi
  // u1phi = R1t.k(sin(delta1),cos(delta1).cos(nu1)-1.,cos(delta1).sin(nu1))
  // u2phi = R2t.k(sin(delta2),cos(delta2).cos(nu2)-1.,cos(delta2).sin(nu2))
  // u3phi = u1phi * u2phi
  // h1phi // u1phi
  // h2phi // P(u1phi,u2phi)
  // Tc =   (h1phi, h2phi, h3phi)
  // Tphi = (u1phi, u2phi, u3phi)
  // Tphi = U.Tc where Tc is orthogonal.
  // U = Tphi * (Tc)t
  double h1 = r1.get_h();
  double h2 = r2.get_h();
  double k1 = r1.get_k();
  double k2 = r2.get_k();
  double l1 = r1.get_l();
  double l2 = r2.get_l();
  svector h1c(h1, k1, l1);
  svector h2c(h2, k2, l2);
  h1c.multiplyOnTheLeft(m_currentCristal.get_B());
  h2c.multiplyOnTheLeft(m_currentCristal.get_B());
  double tau = physicalConstants::getTau();
  double lambda = m_currentSource.getWaveLength();
  double k0 = tau / lambda;

  // Compute matrix Tc from h1c and h2c.
  smatrix Tc;
  h1c.axisSystem(h2c, Tc);
  Tc.transpose();

  svector result;
  h1c.vectorialProduct(h2c,result);
  if (result.norminf() < mathematicalConstants::getEpsilon1())
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
  double nu1    = ((eulerian_angleConfiguration6C*)r1.getAngleConfiguration())->getNu();
  double nu2    = ((eulerian_angleConfiguration6C*)r2.getAngleConfiguration())->getNu();
  double delta1 = ((eulerian_angleConfiguration6C*)r1.getAngleConfiguration())->getDelta();
  double delta2 = ((eulerian_angleConfiguration6C*)r2.getAngleConfiguration())->getDelta();
  svector u1phi(k0*sin(delta1), k0*cos(delta1)*cos(nu1)-k0, k0*cos(delta1)*sin(nu1));
  svector u2phi(k0*sin(delta2), k0*cos(delta2)*cos(nu2)-k0, k0*cos(delta2)*sin(nu2));
  smatrix R1 = computeR(r1.getAngleConfiguration());
  smatrix R2 = computeR(r2.getAngleConfiguration());
  R1.transpose();
  R2.transpose();
  u1phi.multiplyOnTheLeft(R1);
  u2phi.multiplyOnTheLeft(R2);

  // Compute Tphi.
  smatrix Tphi;
  u1phi.axisSystem(u2phi, Tphi);

  u1phi.vectorialProduct(u2phi,result);
  if (result.norminf() < mathematicalConstants::getEpsilon1())
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

// In this function we set the current angle configuration and the
// rotation matrices according to their respective  rotation axes.
void eulerianDiffractometer6C::setAngleConfiguration(angleConfiguration* ac)
{
  double nu    = ((eulerian_angleConfiguration6C*)ac)->getNu();
  double mu    = ((eulerian_angleConfiguration6C*)ac)->getMu();
  double eta   = ((eulerian_angleConfiguration6C*)ac)->getEta();
  double chi   = ((eulerian_angleConfiguration6C*)ac)->getChi();
  double phi   = ((eulerian_angleConfiguration6C*)ac)->getPhi();
  double delta = ((eulerian_angleConfiguration6C*)ac)->getDelta();
  double cos_nu    = cos(nu);
  double sin_nu    = sin(nu);
  double cos_mu    = cos(mu);
  double sin_mu    = sin(mu);
  double cos_eta   = cos(eta);
  double sin_eta   = sin(eta);
  double cos_chi   = cos(chi);
  double sin_chi   = sin(chi);
  double cos_phi   = cos(phi);
  double sin_phi   = sin(phi);
  double cos_delta = cos(delta);
  double sin_delta = sin(delta);

  // Matrix mu
  //  |  1.     0.       0. |
  //  |  0.  cos_mu  -sin_mu|
  //  |  0.  sin_mu   cos_mu|
  m_MU.set(
        1.,    0.,      0.,
        0., cos_mu, -sin_mu,
        0., sin_mu,  cos_mu);

  // Matrix eta stored in matrix omega
  //  |  cos_eta  sin_eta 0. |
  //  | -sin_eta  cos_eta 0. |
  //  |     0.       0.   1. |
  m_OMEGA.set(
     cos_eta, sin_eta, 0.,
    -sin_eta, cos_eta, 0.,
        0.,      0.,   1.);

  // Matrix Chi
  //  |  cos_chi  0.  sin_chi |
  //  |     0.    1.     0.   |
  //  | -sin_chi  0.  cos_chi |
  m_CHI.set(
     cos_chi, 0., sin_chi,
        0.,   1.,    0.,
    -sin_chi, 0., cos_chi);

  // Matrix Phi
  //  |  cos_phi   sin_phi   0. |
  //  | -sin_phi   cos_phi   0. |
  //  |     0.        0.     1. |
  m_PHI.set(
     cos_phi, sin_phi, 0.,
    -sin_phi, cos_phi, 0.,
        0.,      0.,   1.);

  // Matrix nu
  //  |  1.     0.       0. |
  //  |  0.  cos_nu  -sin_nu|
  //  |  0.  sin_nu   cos_nu|
  m_NU.set(
        1.,    0.,      0.,
        0., cos_nu, -sin_nu,
        0., sin_nu,  cos_nu);

  // Matrix Delta stored in matrix 2Theta
  //  |  cos_delta   sin_delta 0. |
  //  | -sin_delta   cos_delta 0. |
  //  |     0.          0.     1. |
  m_2THETA.set(
     cos_delta, sin_delta, 0.,
    -sin_delta, cos_delta, 0.,
        0.,        0.,     1.);
}

smatrix eulerianDiffractometer6C::computeR()
{
  smatrix R(m_MU);
  R.multiplyOnTheRight(m_OMEGA); // matrix ETA
  R.multiplyOnTheRight(m_CHI);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

smatrix eulerianDiffractometer6C::computeR(angleConfiguration* ac)
{
  setAngleConfiguration(ac);
  smatrix R(m_MU);
  R.multiplyOnTheRight(m_OMEGA); // matrix ETA
  R.multiplyOnTheRight(m_CHI);
  R.multiplyOnTheRight(m_PHI);
  return R;
}

void eulerianDiffractometer6C::printOnScreen() const
{
  diffractometer::printOnScreen();
  std::cout << std::endl << "CLASS eulerianDiffractometer6C";
  //m_currentSource.printOnScreen();
  //m_currentCristal.printOnScreen();
}

int test1_eulerian6C()
{
  // First of all we create a light source whose significant parameter is the wave length
  // (the first one). Then we create a cristal, from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The matrix U comes from two reflections which
  // are stored in a diffractometer. Calling computeAngles() triggers the main computations.

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
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections needed to compute U.
  eulerian_angleConfiguration6C* eul6C_1 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., 0., 0., 60.*degToRad);
  eulerian_angleConfiguration6C* eul6C_2 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., -90.*degToRad, 0., 60.*degToRad);
  reflection r1(eul6C_1, 1, 0, 0, reflection::Best);
  reflection r2(eul6C_2, 0, 1, 0, reflection::Best);
  delete eul6C_1;
  delete eul6C_2;

  h = 1;
  k = 0;
  l = 0;
  // After setting (h,k,l) creation of a diffractometer.
  eulerianDiffractometer6C diff_6C_8(
    cubic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C); // mu = nu =0.
    //cubic_cristal1, _source1, r1, r2, mode::diffractometer_mode::horizontal4CBissector6C);
  eulerian_angleConfiguration6C* eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  // Check the value of the angles.
  if (fabs(eac8->getEta() - eac8->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->getChi() - 0.) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->getPhi() - 0.) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return 1;
  }
  if (fabs(eac8->getDelta() - 1.04719755142099) > mathematicalConstants::getEpsilon0())
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
  eulerianDiffractometer6C diff_4C_9(
    cubic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac9 = (eulerian_angleConfiguration6C*)diff_4C_9.computeAngles(h,k,l);
  if (fabs(eac9->getEta() - eac9->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  //if (fabs(eac9->getChi() - 3.14159265358979) > 
  if (fabs(eac9->getChi() - 0.) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  //if (fabs(eac9->getPhi() - 89.9999999882484*degToRad) > 
  if (fabs(eac9->getPhi() + 89.9999999882484*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return 2;
  }
  if (fabs(eac9->getDelta() - 1.04719755142099) > mathematicalConstants::getEpsilon0())
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
  eulerianDiffractometer6C diff_4C_10(
    cubic_cristal1, _source1, r1, r2,mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getEta() - eac10->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->getChi() - 0.615479708670) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->getPhi() + 0.7853981634) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 3;
  }
  if (fabs(eac10->getDelta() - 2.09439510239) > mathematicalConstants::getEpsilon0())
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
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
  if (fabs(eac10->getEta() - eac10->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->getChi() + 0.615479708670) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->getPhi() - 0.7853981634) > mathematicalConstants::getEpsilon0())
  {
    delete eac10;
    return 4;
  }
  if (fabs(eac10->getDelta() - 2.09439510239) > mathematicalConstants::getEpsilon0())
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
  eulerianDiffractometer6C diff_4C_11(
    triclinic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  if (fabs(eac11->getEta() - eac11->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->getChi() + 0.50074641878936) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->getPhi() - 1.1282872259289) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  if (fabs(eac11->getDelta() - 0.23331517265342) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return 5;
  }
  delete eac11;

  /////////////
  // TEST 6 //
  ///////////
  eulerian_angleConfiguration6C* eul4C_3 = new eulerian_angleConfiguration6C(
      0., 20.6255*degToRad, 0., -15.*degToRad, 0., 11.2515*degToRad);
  eulerian_angleConfiguration6C* eul4C_4 = new eulerian_angleConfiguration6C(
      0., 21.3545*degToRad, 0., -72.616*degToRad, 0., 12.7090*degToRad);
  reflection r3(eul4C_3, 1, 0, 0, reflection::Best);
  reflection r4(eul4C_4, 0, 1, 0, reflection::Best);
  delete eul4C_4;
  delete eul4C_3;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer6C diff_4C_3(
    cubic_cristal1, _source1, r3, r4, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac3 = (eulerian_angleConfiguration6C*)diff_4C_3.computeAngles(h,k,l);
  if (fabs(eac3->getEta() - eac3->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->getChi() - 0.61547970867039) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->getPhi() + 0.78540252672058) > mathematicalConstants::getEpsilon0())
  {
    delete eac3;
    return 6;
  }
  if (fabs(eac3->getDelta() - 2.0943951023932) > mathematicalConstants::getEpsilon0())
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
  eulerianDiffractometer6C diff_4C_4(
    triclinic_cristal1, _source1, r3, r4, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac4 = (eulerian_angleConfiguration6C*)diff_4C_4.computeAngles(h,k,l);
  if (fabs(eac4->getEta() - eac4->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->getChi() + 0.50074641878936) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->getPhi() - 1.1282828626057) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  if (fabs(eac4->getDelta() - 0.23331517265342) > mathematicalConstants::getEpsilon0())
  {
    delete eac4;
    return 7;
  }
  delete eac4;

  /////////////
  // TEST 8 //
  ///////////
  eulerian_angleConfiguration6C* eul4C_5 = new eulerian_angleConfiguration6C(
      0., -13.9535*degToRad, -59.5210*degToRad, -12.56*degToRad, 0., 48.8585*degToRad);
  eulerian_angleConfiguration6C* eul4C_6 = new eulerian_angleConfiguration6C(
      0., 45.651*degToRad, 9.6535*degToRad, -55.4835*degToRad, 0., 42.8625*degToRad);
  reflection r5(eul4C_5, -1, 3, 5, reflection::Best);
  reflection r6(eul4C_6, 2, 2, -1, reflection::Best);
  delete eul4C_5;
  delete eul4C_6;
  h = 1;
  k = 1;
  l = -1;
  eulerianDiffractometer6C diff_4C_5(
    cubic_cristal1, _source1, r5, r6, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac5 = (eulerian_angleConfiguration6C*)diff_4C_5.computeAngles(h,k,l);
  if (fabs(eac5->getEta() - eac5->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->getChi() - 0.86804479702510) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->getPhi() + 0.13598637990087) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  if (fabs(eac5->getDelta() - 2.0943951023932) > mathematicalConstants::getEpsilon0())
  {
    delete eac5;
    return 8;
  }
  delete eac5;

  /////////////
  // TEST 9 //
  ///////////
  eulerian_angleConfiguration6C* eul4C_7 = new eulerian_angleConfiguration6C(
      0., -13.9535*degToRad, -59.5210*degToRad, -12.56*degToRad, 0., 48.8585*degToRad);
  eulerian_angleConfiguration6C* eul4C_8 = new eulerian_angleConfiguration6C(
      0., 45.651*degToRad, 9.6535*degToRad, -55.4835*degToRad, 0., 42.8625*degToRad);
  reflection r7(eul4C_7, -1, 3, 5, reflection::Best);
  reflection r8(eul4C_8, 2, 2, -1, reflection::Best);
  delete eul4C_7;
  delete eul4C_8;
  h = 1;
  k = -1;
  l = 1;
  eulerianDiffractometer6C diff_4C_6(
    triclinic_cristal1, _source1, r7, r8, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* _eac9 = (eulerian_angleConfiguration6C*)diff_4C_6.computeAngles(h,k,l);
  if (fabs(_eac9->getEta() - _eac9->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->getChi() + 0.50074572470125) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->getPhi() - 1.1282833085965) > mathematicalConstants::getEpsilon0())
  {
    delete _eac9;
    return 9;
  }
  if (fabs(_eac9->getDelta() - 0.23331517265342) > mathematicalConstants::getEpsilon0())
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
  source _source2(0.7093,2.36,5.68);
  eulerian_angleConfiguration6C* eul4C_3_ = new eulerian_angleConfiguration6C(
      0., 2.542*degToRad, -26.15*degToRad, 92.925*degToRad, 0., 5.044*degToRad);
  eulerian_angleConfiguration6C* eul4C_4_ = new eulerian_angleConfiguration6C(
      0., 2.538*degToRad, 71.19*degToRad, -12.37*degToRad, 0., 5.095*degToRad);
  reflection r3_(eul4C_3_, 2, -2, 0, reflection::Best);
  reflection r4_(eul4C_4_, -2, 0, 0, reflection::Best);
  delete eul4C_3_;
  delete eul4C_4_;
  h = 10;
  k = -8;
  l = 4;
  eulerianDiffractometer6C diff_4C_10_(
    triclinic_cristal2, _source2, r3_, r4_, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac10_ = (eulerian_angleConfiguration6C*)diff_4C_10_.computeAngles(h,k,l);
  if (fabs(eac10_->getEta() - eac10_->getDelta()/2.) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->getChi() - 3.5967851323293) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->getPhi() + 1.0667584081915) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  if (fabs(eac10_->getDelta() - 0.43899437027362) > mathematicalConstants::getEpsilon0())
  {
    delete eac10_;
    return 10;
  }
  delete eac10_;

  return 0;
}

int test2_eulerian6C()
{
  // First of all we create a light source whose significant parameter is the wave length
  // (the first one). Then we create a cristal, from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The matrix U comes from two reflections which
  // are stored in a diffractometer. Calling computeAngles() triggers the main computations.

  /////////////
  // TEST 11 //
  ///////////
  int ii = 11;
  double h, k, l;
  double H, K, L;
  // Creation of a light source.
  source _source1(1.54,2.36,5.68);
  // Creation of a crystal.
  cristal cubic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections needed to compute U.
  eulerian_angleConfiguration6C* eul6C_1 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., 0., 0., 60.*degToRad);
  eulerian_angleConfiguration6C* eul6C_2 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., 90.*degToRad, 0., 60.*degToRad);
  reflection r1(eul6C_1, 1, 0, 0, reflection::Best);
  reflection r2(eul6C_2, 0, 1, 0, reflection::Best);
  delete eul6C_1;
  delete eul6C_2;

  h = 1;
  k = 1;
  l = 1;
  // After setting (h,k,l) creation of a diffractometer.
  eulerianDiffractometer6C diff_6C_8(
    cubic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C); // mu = nu =0.
  eulerian_angleConfiguration6C* eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  // Check the value of the angles.
  if (fabs(eac8->getEta() - 60.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getChi() - 35.264389682755*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getPhi() - 45.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getDelta() - 120.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  //////////////
  // TEST 12 //
  ////////////
  ii = 12;
  h = -1.78;
  k = 0.1;
  l = 0.45;
  // Compute angles from (h,k,l).
  eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  if (fabs(eac8->getEta() - 66.832870414616*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getChi() - 165.83381747630*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getPhi() + 3.2154839917482*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getDelta() - 133.66574082923*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  // Compute (H,K,L)  from angles and check their values.
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  //////////////
  // TEST 13 //
  ////////////
  ii = 13;
  h = 0.;
  k = 0.;
  l = 2.;
  // Compute angles from (h,k,l).
  eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  if (fabs(eac8->getEta() - 90.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getChi() - 90.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  // This value of phi has not been computed through atan2 because both
  // hphi1 and hphi2 are null so it has been arbitrarily set to 0.
  if (fabs(eac8->getPhi() - 0.) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getDelta() - 180.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  // Compute (H,K,L)  from angles and check their values.
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  //////////////
  // TEST 14 //
  ////////////
  h = 1;
  k = -1;
  l = 1;
  ii = 14;
  cristal triclinic_cristal1(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  eulerian_angleConfiguration6C* eul4C_3_ = new eulerian_angleConfiguration6C(
      0., 2.542*degToRad, -26.15*degToRad, 92.925*degToRad, 0., 5.044*degToRad);
  eulerian_angleConfiguration6C* eul4C_4_ = new eulerian_angleConfiguration6C(
      0., 2.538*degToRad, 71.19*degToRad, -12.37*degToRad, 0., 5.095*degToRad);
  reflection r3_(eul4C_3_, 2, -2, 0, reflection::Best);
  reflection r4_(eul4C_4_, -2, 0, 0, reflection::Best);
  delete eul4C_3_;
  delete eul4C_4_;
  eulerianDiffractometer6C diff_4C_11(
    triclinic_cristal1, _source1, r3_, r4_, mode::vertical4CBissector6C);
  // Compute angles from (h,k,l).
  eulerian_angleConfiguration6C* eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  if (fabs(eac11->getEta() - 6.6839873447035*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getChi() - 195.57068360582*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getPhi() + 58.267356415193*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getDelta() - 13.367974689407*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  // Compute (H,K,L)  from angles and check their values.
  diff_4C_11.computeHKL(H,K,L,eac11);
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
  // TEST 15 //
  ////////////
  h = -1.78;
  k = 0.1;
  l = 0.45;
  ii = 15;
  // Compute angles from (h,k,l).
  eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  if (fabs(eac11->getEta() - 9.7012454956313*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getChi() - 79.708968252753*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getPhi() - 3.4507676746575*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  if (fabs(eac11->getDelta() - 19.402490991263*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac11;
    return ii;
  }
  // Compute (H,K,L)  from angles and check their values.
  diff_4C_11.computeHKL(H,K,L,eac11);
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


  return 0;
}

int test3_eulerian6C()
{
  // First of all we create a light source whose significant parameter is the wave length
  // (the first one). Then we create a cristal, from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The matrix U comes from two reflections which
  // are stored in a diffractometer. Calling computeAngles() triggers the main computations.

  /////////////
  // TEST 21 //
  ///////////
  int ii = 21;
  double h, k, l;
  double H, K, L;
  // Creation of a light source.
  source _source1(1.54,2.36,5.68);
  // Creation of a crystal.
  cristal cubic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections needed to compute U.
  eulerian_angleConfiguration6C* eul6C_1 = new eulerian_angleConfiguration6C(
    30.*degToRad, 0., 0., 0., 60.*degToRad, 0.);
  eulerian_angleConfiguration6C* eul6C_2 = new eulerian_angleConfiguration6C(
    30.*degToRad, 0., -90.*degToRad, 90.*degToRad, 60.*degToRad, 0.);
  reflection r1(eul6C_1, 1, 0, 0, reflection::Best);
  reflection r2(eul6C_2, 0, 1, 0, reflection::Best);
  delete eul6C_1;
  delete eul6C_2;

  h = 1;
  k = 1;
  l = 1;
  // After setting (h,k,l) creation of a diffractometer.
  eulerianDiffractometer6C diff_6C_8(
    cubic_cristal1, _source1, r1, r2, mode::horizontal4CBissector6C); // eta = delta =0.
  // Check the value of the angles.
  eulerian_angleConfiguration6C* eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  /*
  if (fabs(eac8->getMu() - 60.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getChi() - 35.264389682755*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getPhi() - 45.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getNu() - 120.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  */
  // Compute (H,K,L)  from angles and check their values.
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  /////////////
  // TEST 22 //
  ///////////
  h = 1;
  k = 1;
  l = 1;
  ii = 22;
  eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  /*
  if (fabs(eac8->getMu() - 60.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getChi() - 35.264389682755*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getPhi() - 45.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(eac8->getNu() - 120.*degToRad) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  */
  // Compute (H,K,L)  from angles and check their values.
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  /////////////
  // TEST 23 //
  ///////////
  h = 1;
  k = 1;
  l = 1;
  ii = 23;
  eulerian_angleConfiguration6C* eul4C_3 = new eulerian_angleConfiguration6C(
      30.*degToRad, 0., 0., 0., 60.*degToRad, 0.);
  eulerian_angleConfiguration6C* eul4C_4 = new eulerian_angleConfiguration6C(
      120.*degToRad, 0., 0., 0., 60.*degToRad, 0.);
  reflection r3(eul4C_3, 1, 0, 0, reflection::Best);
  reflection r4(eul4C_4, 0, 1, 0, reflection::Best);
  delete eul4C_4;
  delete eul4C_3;
  eulerianDiffractometer6C diff_4C_3(
    cubic_cristal1, _source1, r3, r4, mode::horizontal4CBissector6C);
  eulerian_angleConfiguration6C* eac3 = (eulerian_angleConfiguration6C*)diff_4C_3.computeAngles(h,k,l);
  diff_4C_3.computeHKL(H,K,L,eac3);
  /*
  double phi_deg = eac3->getPhi()*mathematicalConstants::convertAnglesToDegrees();
  double chi_deg = eac3->getChi()*mathematicalConstants::convertAnglesToDegrees();
  double eta_deg = eac3->getEta()*mathematicalConstants::convertAnglesToDegrees();
  double mu_deg  = eac3->getMu()*mathematicalConstants::convertAnglesToDegrees();
  double del_deg = eac3->getDelta()*mathematicalConstants::convertAnglesToDegrees();
  double nu_deg  = eac3->getNu()*mathematicalConstants::convertAnglesToDegrees();
  */
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

  return 0;
}

int test4_eulerian6C()
{
  // First of all we create a light source whose significant parameter is the wave length
  // (the first one). Then we create a cristal, from its direct lattice we compute the
  // reciprocal lattice and the matrix B. The matrix U comes from two reflections which
  // are stored in a diffractometer. Calling computeAngles() triggers the main computations.

  //////////////
  // TEST 31 //
  ////////////
  int ii = 31;
  double h, k, l;
  double H, K, L;
  // Creation of a light source.
  source _source1(1.54,2.36,5.68);
  // Creation of a crystal.
  cristal cubic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.54, 1.54, 1.54);
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  // Creation of the two basic non parallel reflections needed to compute U.
  eulerian_angleConfiguration6C* eul6C_1 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., 0., 0., 60.*degToRad);
  eulerian_angleConfiguration6C* eul6C_2 = new eulerian_angleConfiguration6C(0., 30.*degToRad, 0., -90.*degToRad, 0., 60.*degToRad);
  reflection r1(eul6C_1, 1, 0, 0, reflection::Best);
  reflection r2(eul6C_2, 0, 1, 0, reflection::Best);
  delete eul6C_1;
  delete eul6C_2;

  h = 1;
  k = 0;
  l = 0;
  // After setting (h,k,l) creation of a diffractometer. In vertical 4C mode : mu = nu =0.
  eulerianDiffractometer6C diff_6C_8(cubic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C);
    //cubic_cristal1, _source1, r1, r2, mode::diffractometer_mode::horizontal4CBissector6C);
  eulerian_angleConfiguration6C* eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  // Check the value of the angles.
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  // Horizontal 4C bissector mode.
  diff_6C_8.setMode(mode::horizontal4CBissector6C);
  eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;

  /*
  // Lifting 3C detector mode.
  // The scattering vector is perpendicular to the light ray.
  diff_6C_8.setMode(mode::diffractometer_mode::lifting3CDetector6C);
  eac8 = (eulerian_angleConfiguration6C*)diff_6C_8.computeAngles(h,k,l);
  diff_6C_8.computeHKL(H,K,L,eac8);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac8;
    return ii;
  }
  delete eac8;
  */

  //////////////
  // TEST 32 //
  ////////////
  h = 0;
  k = 1;
  l = 0;
  ii = 32;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_9(cubic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac9 = (eulerian_angleConfiguration6C*)diff_4C_9.computeAngles(h,k,l);
  diff_4C_9.computeHKL(H,K,L,eac9);
  if (fabs(H - h) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  //if (fabs(eac9->getChi() - 3.14159265358979) > 
  if (fabs(K - k) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  //if (fabs(eac9->getPhi() - 89.9999999882484*degToRad) > 
  if (fabs(L - l) > mathematicalConstants::getEpsilon0())
  {
    delete eac9;
    return ii;
  }
  delete eac9;

  // Horizontal 4C bissector mode.
  diff_4C_9.setMode(mode::horizontal4CBissector6C);
  eac9 = (eulerian_angleConfiguration6C*)diff_4C_9.computeAngles(h,k,l);
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

  // Lifting 3C detector mode.
  diff_4C_9.setMode(mode::lifting3CDetector6C);
  eac9 = (eulerian_angleConfiguration6C*)diff_4C_9.computeAngles(h,k,l);
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
  h = 1;
  k = 1;
  l = -1;
  ii = 33;
  eulerianDiffractometer6C diff_4C_10(
    cubic_cristal1, _source1, r1, r2,mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_10.setMode(mode::horizontal4CBissector6C);
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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

  /*
  // Unreachable reflection with the mu circle !
  // Lifting 3C detector mode.
  diff_4C_10.setMode(mode::diffractometer_mode::lifting3CDetector6C);
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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
  */

  //////////////
  // TEST 34 //
  ////////////
  h = 1;
  k = -1;
  l = 1;
  ii = 34;
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_10.setMode(mode::horizontal4CBissector6C);
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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

  /*
  // Lifting 3C detector mode.
  // Mu circle cannot reach the diffraction position
  // cos²(mu) + sin²(mu) > 1.
  diff_4C_10.setMode(mode::diffractometer_mode::lifting3CDetector6C);
  eac10 = (eulerian_angleConfiguration6C*)diff_4C_10.computeAngles(h,k,l);
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
  */

  //////////////
  // TEST 35 //
  ////////////
  h = 1;
  k = -1;
  l = 1;
  ii = 35;
  cristal triclinic_cristal1(
    91.23  * mathematicalConstants::getPI() / 180.,
    93.64  * mathematicalConstants::getPI() / 180.,
    122.21 * mathematicalConstants::getPI() / 180.,
    9.32, 8.24, 13.78);
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_11(triclinic_cristal1, _source1, r1, r2, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  diff_4C_11.computeHKL(H,K,L,eac11);
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

  // Horizontal 4C bissector mode.
  diff_4C_11.setMode(mode::horizontal4CBissector6C);
  eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  diff_4C_11.computeHKL(H,K,L,eac11);
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

  // Lifting 3C detector mode.
  diff_4C_11.setMode(mode::lifting3CDetector6C);
  eac11 = (eulerian_angleConfiguration6C*)diff_4C_11.computeAngles(h,k,l);
  diff_4C_11.computeHKL(H,K,L,eac11);
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
  eulerian_angleConfiguration6C* eul4C_3 = new eulerian_angleConfiguration6C(
      0., 20.6255*degToRad, 0., -15.*degToRad, 0., 11.2515*degToRad);
  eulerian_angleConfiguration6C* eul4C_4 = new eulerian_angleConfiguration6C(
      0., 21.3545*degToRad, 0., -72.616*degToRad, 0., 12.7090*degToRad);
  reflection r3(eul4C_3, 1, 0, 0, reflection::Best);
  reflection r4(eul4C_4, 0, 1, 0, reflection::Best);
  delete eul4C_4;
  delete eul4C_3;
  h = 1;
  k = 1;
  l = -1;
  ii = 36;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_3(
    cubic_cristal1, _source1, r3, r4, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac3 = (eulerian_angleConfiguration6C*)diff_4C_3.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_3.setMode(mode::horizontal4CBissector6C);
  eac3 = (eulerian_angleConfiguration6C*)diff_4C_3.computeAngles(h,k,l);
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

  /*
  cos(nu) > 1.
  // Lifting 3C detector mode.
  diff_4C_3.setMode(mode::diffractometer_mode::lifting3CDetector6C);
  eac3 = (eulerian_angleConfiguration6C*)diff_4C_3.computeAngles(h,k,l);
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
  */

  //////////////
  // TEST 37 //
  ////////////
  h = 1;
  k = -1;
  l = 1;
  ii = 37;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_4(
    triclinic_cristal1, _source1, r3, r4, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac4 = (eulerian_angleConfiguration6C*)diff_4C_4.computeAngles(h,k,l);
  diff_4C_4.computeHKL(H,K,L,eac4);
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

  // Horizontal 4C bissector mode.
  diff_4C_4.setMode(mode::horizontal4CBissector6C);
  eac4 = (eulerian_angleConfiguration6C*)diff_4C_4.computeAngles(h,k,l);
  diff_4C_4.computeHKL(H,K,L,eac4);
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

  // Lifting 3C detector mode.
  diff_4C_4.setMode(mode::lifting3CDetector6C);
  eac4 = (eulerian_angleConfiguration6C*)diff_4C_4.computeAngles(h,k,l);
  diff_4C_4.computeHKL(H,K,L,eac4);
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
  eulerian_angleConfiguration6C* eul4C_5 = new eulerian_angleConfiguration6C(
      0., -13.9535*degToRad, -59.5210*degToRad, -12.56*degToRad, 0., 48.8585*degToRad);
  eulerian_angleConfiguration6C* eul4C_6 = new eulerian_angleConfiguration6C(
      0., 45.651*degToRad, 9.6535*degToRad, -55.4835*degToRad, 0., 42.8625*degToRad);
  reflection r5(eul4C_5, -1, 3, 5, reflection::Best);
  reflection r6(eul4C_6, 2, 2, -1, reflection::Best);
  delete eul4C_5;
  delete eul4C_6;
  h = 1;
  k = 1;
  l = -1;
  ii = 38;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_5(
    cubic_cristal1, _source1, r5, r6, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac5 = (eulerian_angleConfiguration6C*)diff_4C_5.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_5.setMode(mode::horizontal4CBissector6C);
  eac5 = (eulerian_angleConfiguration6C*)diff_4C_5.computeAngles(h,k,l);
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

  /*
  sin(delta) > 1.
  // Lifting 3C detector mode.
  diff_4C_5.setMode(mode::diffractometer_mode::lifting3CDetector6C);
  eac5 = (eulerian_angleConfiguration6C*)diff_4C_5.computeAngles(h,k,l);
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
  */

  //////////////
  // TEST 39 //
  ////////////
  eulerian_angleConfiguration6C* eul4C_7 = new eulerian_angleConfiguration6C(
      0., -13.9535*degToRad, -59.5210*degToRad, -12.56*degToRad, 0., 48.8585*degToRad);
  eulerian_angleConfiguration6C* eul4C_8 = new eulerian_angleConfiguration6C(
      0., 45.651*degToRad, 9.6535*degToRad, -55.4835*degToRad, 0., 42.8625*degToRad);
  reflection r7(eul4C_7, -1, 3, 5, reflection::Best);
  reflection r8(eul4C_8, 2, 2, -1, reflection::Best);
  delete eul4C_7;
  delete eul4C_8;
  h = 1;
  k = -1;
  l = 1;
  ii = 39;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_6(
    triclinic_cristal1, _source1, r7, r8, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* _eac9 = (eulerian_angleConfiguration6C*)diff_4C_6.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_6.setMode(mode::horizontal4CBissector6C);
  _eac9 = (eulerian_angleConfiguration6C*)diff_4C_6.computeAngles(h,k,l);
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

  // Lifting 3C detector mode.
  diff_4C_6.setMode(mode::horizontal4CBissector6C);
  _eac9 = (eulerian_angleConfiguration6C*)diff_4C_6.computeAngles(h,k,l);
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
  cristal triclinic_cristal2(
    89.990 * mathematicalConstants::getPI() / 180.,
    89.963 * mathematicalConstants::getPI() / 180.,
    119.99 * mathematicalConstants::getPI() / 180.,
    18.423, 18.417, 18.457);
  source _source2(0.7093,2.36,5.68);
  // eulerian_angleConfiguration6C = (mu, eta, chi, phi, nu, delta)
  eulerian_angleConfiguration6C* eul4C_3_ = new eulerian_angleConfiguration6C(
      0., 2.542*degToRad, -26.15*degToRad, 92.925*degToRad, 0., 5.044*degToRad);
  eulerian_angleConfiguration6C* eul4C_4_ = new eulerian_angleConfiguration6C(
      0., 2.538*degToRad, 71.19*degToRad, -12.37*degToRad, 0., 5.095*degToRad);
  reflection r3_(eul4C_3_, 2, -2, 0, reflection::Best);
  reflection r4_(eul4C_4_, -2, 0, 0, reflection::Best);
  delete eul4C_3_;
  delete eul4C_4_;
  h = 10;
  k = -8;
  l = 4;
  ii = 40;
  // Vertical 4C bissector mode.
  eulerianDiffractometer6C diff_4C_10_(
    triclinic_cristal2, _source2, r3_, r4_, mode::vertical4CBissector6C);
  eulerian_angleConfiguration6C* eac10_ = (eulerian_angleConfiguration6C*)diff_4C_10_.computeAngles(h,k,l);
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

  // Horizontal 4C bissector mode.
  diff_4C_10_.setMode(mode::horizontal4CBissector6C);
  eac10_ = (eulerian_angleConfiguration6C*)diff_4C_10_.computeAngles(h,k,l);
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

  // Lifting 3C detector mode.
  diff_4C_10_.setMode(mode::horizontal4CBissector6C);
  eac10_ = (eulerian_angleConfiguration6C*)diff_4C_10_.computeAngles(h,k,l);
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

int eulerianDiffractometer6C::test_eulerian6C()
{
  // Test angles with full constructors in vertical4CBissector6C.
  int a = test1_eulerian6C();
  if (0 != a)
    return a;

  // Test angle values comparing to psic.
  int b = test2_eulerian6C();
  if (0 != b)
    return b;

  // Test computations in horizontal4CBissector6C.
  int c = test3_eulerian6C();
  if (0 != c)
    return c;

  // Tests angles from (h,k,l) and then (h,k,l) from these angles in vertical4CBissector6C
  // then in horizontal4CBissector6C and lifting3CDetector6C.
  int d = test4_eulerian6C();
  return d;
}
