#include <math.h>
#include "mode.h"
#include "svecmat.h"
#include "constants.h"
#include "angleconfig.h"
#include "HKLException.h"


mode::mode()
{}

mode::~mode()
{}

void mode::printOnScreen() const
{}

eulerian_mode::eulerian_mode()
{}

eulerian_mode::~eulerian_mode()
{}

void eulerian_mode::printOnScreen() const
{}

eulerian_bissectorMode4C::eulerian_bissectorMode4C()
{}

eulerian_bissectorMode4C::~eulerian_bissectorMode4C()
{}

// Solving equation (19) from :
// William R. Busing and Henri A. Levy "Angle calculation
// for 3- and 4- Circle X-ray and Neutron Diffractometer"
// (1967) Acta Cryst., 22, 457-464.
// R11 * hphi1 + R12 * hphi2 + R13 * hphi3 = q
// R21 * hphi1 + R22 * hphi2 + R23 * hphi3 = 0.
// R31 * hphi1 + R32 * hphi2 + R33 * hphi3 = 0.
//
// hphi1 = q(-sin(omega)*sin(phi)+cos(omega)*cos(chi)*cos(phi))
// hphi2 = q( sin(omega)*cos(phi)+cos(omega)*cos(chi)*sin(phi))
// hphi3 = q*cos(omega)*sin(chi)
//
// If omega is constant :
// chi = arcsin(hphi3 / q*cos(omega))
// sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
// cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
// D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) + sin(omega)*sin(omega)]
angleConfiguration*
  eulerian_bissectorMode4C::computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const
  throw (HKLException)
{
  // h(theta) = R.hphi
  double chi;
  double phi;
  double omega;
  double sin_theta;
  double two_theta;
  double hphi_length;
  svector hphi_unitVector;
  svector hphi(h,k,l);
  eulerian_angleConfiguration4C* ac4C = 0;


  hphi.multiplyOnTheLeft(UB);
  hphi.unitVector(hphi_unitVector, hphi_length);

  if (fabs(lambda) < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "lamdba is null",
      "The wave length has not been set",
      "eulerian_bissectorMode4C::computeAngles()");

  if ((fabs(h) < mathematicalConstants::getEpsilon1()) && 
      (fabs(k) < mathematicalConstants::getEpsilon1()) &&
      (fabs(l) < mathematicalConstants::getEpsilon1()))
    throw HKLException(
      "(h,k,l) is null",
      "check your parameters",
      "eulerian_bissectorMode4C::computeAngles()");

  if (hphi.norminf() < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "hphi is null",
      "The matrix U has been computed from two parallel reflections or the crystal matrix is null",
      "eulerian_bissectorMode4C::computeAngles()");

  /////////////////////
  // Bragg relation //
  ///////////////////
  // sin(theta) = || q || * lambda * 0.5 / tau.
  sin_theta = hphi_length * lambda * 0.5;
  // We have to be consistent with the conventions 
  // previously defined when we computed the crystal 
  // reciprocal lattice.
  sin_theta = sin_theta / physicalConstants::getTau();

  if (fabs(sin_theta) > 1.)
    throw HKLException(
      "sine bigger than 1.",
      "hphi_length too big, maybe error in UB matrix",
      "eulerian_bissectorMode4C::computeAngles()");

  two_theta = 2. * asin(sin_theta);

  //omega = asin(sin_theta);
  // By definition in bisector mode.
  omega = 0.;


  double so = sin(omega);
  double co = cos(omega);
  //double so = sin(omega_prime);
  //double co = cos(omega_prime);

  // SPEC limit case.
  if (fabs(co) < mathematicalConstants::getEpsilon1())
  {
    ac4C = new eulerian_angleConfiguration4C;
    phi = atan2(-so * hphi.get_X(), so * hphi.get_Y());
    ac4C->setOmega(omega);
    ac4C->setPhi(phi);
    ac4C->setChi(0.);
    return ac4C;
  }
  double sx = hphi.get_Z() / (hphi_length * co);
  // SPEC : without hphi_length
  //double sx = hphi.get_Z() / (co);

  if (fabs(sx) > 1.)
    throw HKLException(
      "sine bigger than 1.",
      "hphi.getZ() too big or hphi_length too small, maybe error in UB matrix",
      "eulerian_bissectorMode4C::computeAngles()");

  chi = asin(sx);
  // asin() returns values between -PI/2. and PI/2.
  // hphi 1st component sign tells whether chi belongs
  // or not to the other half of the circle i.e. between
  // PI/2. and 3PI/2.
  if (hphi.get_X() < -mathematicalConstants::getEpsilon1())
    chi = mathematicalConstants::getPI() - chi;

  double t = -so*hphi.get_X() + co*cos(chi)*hphi.get_Y();
  double u =  so*hphi.get_Y() + co*cos(chi)*hphi.get_X();

  if ((fabs(t) < mathematicalConstants::getEpsilon1()) && 
      (fabs(u) < mathematicalConstants::getEpsilon1()))
    phi = 0.;
  else
    phi = atan2(t,u);

  // RAFIN.
  //if (thetaIsNegative == TRUE)
  //{
    //chi = chi * (-1);
    //phi = 3.141592654 - phi;
    //omega = omega * (-1);
    //two_theta = two_theta * (-1);
  //}

  ac4C = new eulerian_angleConfiguration4C;
  ac4C->set2Theta(two_theta);
  ac4C->setOmega(omega);
  ac4C->setPhi(phi);
  ac4C->setChi(chi);

  return ac4C;
}

// This method has been designed for testing purposes and is based on a geometric approach to find the angle configuration.
angleConfiguration*
  eulerian_bissectorMode4C::computeAngles_Rafin(
  double h, double k, double l, 
  const smatrix& UB,
  double lambda) const
  throw (HKLException)
{
  // h(theta) = R.hphi
  double two_theta;
  double omega;
  double chi;
  double phi;
  double sin_theta;
  double hphi_length;
  svector hphi_unitVector;
  svector hphi(h,k,l);

  hphi.multiplyOnTheLeft(UB);
  hphi.unitVector(hphi_unitVector, hphi_length);

  /////////////////////
  // Bragg relation //
  ///////////////////
  // sin(theta) = || q || * lambda * 0.5
  sin_theta = hphi_length * lambda * 0.5;
  // We have to be consistent with the conventions previously defined when we computed the crystal reciprocal lattice.
  sin_theta = sin_theta / physicalConstants::getTau();

  if (fabs(sin_theta) > 1.)
    throw HKLException(
      "sine bigger than 1.",
      "hphi.getZ() too big or hphi_length too small, maybe error in UB matrix",
      "eulerian_bissectorMode4C::computeAngles_Rafin()");

  two_theta = 2. * asin(sin_theta);

  omega = asin(sin_theta);

  double length_xOy = sqrt(hphi.get_X()*hphi.get_X() + hphi.get_Y()*hphi.get_Y());

  if ((fabs(hphi.get_Z()) < mathematicalConstants::getEpsilon1()) && 
      (fabs(length_xOy) < mathematicalConstants::getEpsilon1()))
    throw HKLException(
      "atan2 with both null parameters",
      "hphi too small, maybe error in UB matrix",
      "eulerian_bissectorMode4C::computeAngles_Rafin()");
  
  chi = atan2(hphi.get_Z(), length_xOy);
  
  if ((fabs(hphi.get_Y()) < mathematicalConstants::getEpsilon1()) && 
      (fabs(hphi.get_X()) < mathematicalConstants::getEpsilon1()))
  {
    if (fabs(hphi.get_Z()) < mathematicalConstants::getEpsilon1())
     throw HKLException(
       "hphi is null",
       "(h,k,l)=(0,0,0) or error in UB matrix",
       "eulerian_bissectorMode4C::computeAngles_Rafin()");
    else
      phi = 0.;
  }
  else
    phi = atan2(hphi.get_Y(), hphi.get_X());

  eulerian_angleConfiguration4C* ac4C =
    new eulerian_angleConfiguration4C;
  ac4C->set2Theta(two_theta);
  ac4C->setOmega(0.); // bisector mode !
  ac4C->setPhi(phi);
  ac4C->setChi(chi);

  return ac4C;
}

// Compute (h,k,l) from a sample of angles.
// Solve a linear system Ax = b where A is the product of the rotation matrices 
// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
void eulerian_bissectorMode4C::computeHKL(
  double& h, double& k, double& l, const smatrix& UB, 
    double lambda, angleConfiguration* ac) const
  throw (HKLException)
{
  double chi       = ((eulerian_angleConfiguration4C*)ac)->getChi();
  double phi       = ((eulerian_angleConfiguration4C*)ac)->getPhi();
  double omega     = ((eulerian_angleConfiguration4C*)ac)->getOmega();
  double two_theta = ((eulerian_angleConfiguration4C*)ac)->get2Theta();

  omega = omega - two_theta/2.; // bisector mode !

  double cos_chi       = cos(chi);
  double sin_chi       = sin(chi);
  double cos_phi       = cos(phi);
  double sin_phi       = sin(phi);
  double cos_omega     = cos(omega);
  double sin_omega     = sin(omega);

  smatrix OMEGA;
  smatrix CHI;
  smatrix PHI;

  // Matrix Omega
  //  | cos_omega   sin_omega 0. |
  //  |-sin_omega   cos_omega 0. |
  //  |      0.        0.     1. |
  OMEGA.set(
     cos_omega, sin_omega, 0.,
    -sin_omega, cos_omega, 0.,
        0.,        0.,     1.);

  // Matrix Chi
  //  |  cos_chi  0.  sin_chi |
  //  |   0.      1.    0.    |
  //  | -sin_chi  0.  cos_chi |
  CHI.set(
    cos_chi, 0., sin_chi,
       0.,   1.,    0.,
   -sin_chi, 0., cos_chi);

  // Matrix Phi
  //  |  cos_phi   sin_phi   0. |
  //  | -sin_phi   cos_phi   0. |
  //  |      0.        0.    1. |
  PHI.set(
     cos_phi, sin_phi, 0.,
    -sin_phi, cos_phi, 0.,
        0.,      0.,   1.);

  smatrix A(OMEGA);
  A.multiplyOnTheRight(CHI);
  A.multiplyOnTheRight(PHI);
  A.multiplyOnTheRight(UB);


  double det1 = A.get(1,1)*(A.get(2,2)*A.get(3,3)-A.get(3,2)*A.get(2,3));
  double det2 =-A.get(1,2)*(A.get(2,1)*A.get(3,3)-A.get(3,1)*A.get(2,3));
  double det3 = A.get(1,3)*(A.get(2,1)*A.get(3,2)-A.get(3,1)*A.get(2,2));
  double det = det1 + det2 + det3;

  if (fabs(det) < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "det(A) is null",
      "A = OME*CHI*PHI*U*B check if one of these matrices is null",
      "eulerian_bissectorMode4C::computeHKL()");

  double sin_theta = sin(two_theta*0.5);
  // We have to be consistent with the conventions previously defined 
  // when we chose tau. q = 2tau * sin(theta) / lambda.
  double q = (2. * sin_theta * physicalConstants::getTau()) / lambda;

  h = q * (A.get(2,2)*A.get(3,3)-A.get(3,2)*A.get(2,3)) / det;

  k =-q * (A.get(2,1)*A.get(3,3)-A.get(3,1)*A.get(2,3)) / det;

  l = q * (A.get(2,1)*A.get(3,2)-A.get(3,1)*A.get(2,2)) / det;
}

void eulerian_bissectorMode4C::printOnScreen() const
{}


//****************************
//*** CONSTANT OMEGA MODE ***
//**************************
eulerian_constantOmegaMode4C::eulerian_constantOmegaMode4C(double constantOmega)
{
  m_constantOmega = constantOmega;
}

eulerian_constantOmegaMode4C::~eulerian_constantOmegaMode4C()
{}

// Solving equation (19) from :
// William R. Busing and Henri A. Levy "Angle calculation for 3- and 4- Circle X-ray and Neutron Diffractometer"
// (1967) Acta Cryst., 22, 457-464.
// R11 * hphi1 + R12 * hphi2 + R13 * hphi3 = q
// R21 * hphi1 + R22 * hphi2 + R23 * hphi3 = 0.
// R31 * hphi1 + R32 * hphi2 + R33 * hphi3 = 0.
//
// hphi1 = q(-sin(omega)*sin(phi)+cos(omega)*cos(chi)*cos(phi))
// hphi2 = q( sin(omega)*cos(phi)+cos(omega)*cos(chi)*sin(phi))
// hphi3 = q*cos(omega)*sin(chi)
//
// If omega is constant :
// chi = arcsin(hphi3 / q*cos(omega))
// sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
// cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
// D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) + sin(omega)*sin(omega)]
angleConfiguration*
  eulerian_constantOmegaMode4C::computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const
  throw (HKLException)
{
  // h(theta) = R.hphi
  double chi;
  double phi;
  double omega;
  double two_theta;
  double sin_theta;
  double hphi_length;
  svector hphi(h,k,l);
  svector hphi_unitVector;
  eulerian_angleConfiguration4C* ac4C = 0;


  hphi.multiplyOnTheLeft(UB);
  hphi.unitVector(hphi_unitVector, hphi_length);

  if (fabs(lambda) < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "lamdba is null",
      "The wave length has not been set",
      "eulerian_constantOmegaMode4C::computeAngles()");

  if ((fabs(h) < mathematicalConstants::getEpsilon1()) && 
      (fabs(k) < mathematicalConstants::getEpsilon1()) &&
      (fabs(l) < mathematicalConstants::getEpsilon1()))
    throw HKLException(
      "(h,k,l) is null",
      "check your parameters",
      "eulerian_constantOmegaMode4C::computeAngles()");

  if (hphi.norminf() < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "hphi is null",
      "The matrix U has been computed from two parallel reflections or the crystal matrix is null",
      "eulerian_constantOmegaMode4C::computeAngles()");

  /////////////////////
  // Bragg relation //
  ///////////////////
  // sin(theta) = || q || * lambda * 0.5 / tau.
  sin_theta = hphi_length * lambda * 0.5;
  // We have to be consistent with the conventions 
  // previously defined when we computed the crystal 
  // reciprocal lattice.
  sin_theta = sin_theta / physicalConstants::getTau();

  if (fabs(sin_theta) > 1.)
    throw HKLException(
      "sine bigger than 1.",
      "hphi_length too big, maybe error in UB matrix",
      "eulerian_constantOmegaMode4C::computeAngles()");

  two_theta = 2. * asin(sin_theta);

  // By definition in 4C omega constant mode.
  omega = m_constantOmega;


  double so = sin(omega);
  double co = cos(omega);

  // SPEC limit case.
  if (fabs(co) < mathematicalConstants::getEpsilon1())
  {
    ac4C = new eulerian_angleConfiguration4C;
    phi = atan2(-so * hphi.get_X(), so * hphi.get_Y());
    ac4C->setOmega(omega);
    ac4C->setPhi(phi);
    ac4C->setChi(0.);
    return ac4C;
  }
  double sx = hphi.get_Z() / (hphi_length * co);
  // SPEC : without hphi_length
  //double sx = hphi.get_Z() / (co);

  if (fabs(sx) > 1.)
    throw HKLException(
      "sine bigger than 1.",
      "hphi.getZ() too big or hphi_length too small, maybe error in UB matrix",
      "eulerian_constantOmegaMode4C::computeAngles()");

  chi = asin(sx);
  // asin() returns values between -PI/2. and PI/2. hphi 1st component sign tells whether chi belongs
  // or not to the other half of the circle i.e. between PI/2. and 3PI/2.
  if (hphi.get_X() < -mathematicalConstants::getEpsilon1())
    chi = mathematicalConstants::getPI() - chi;

  double t = -so*hphi.get_X() + co*cos(chi)*hphi.get_Y();
  double u =  so*hphi.get_Y() + co*cos(chi)*hphi.get_X();

  if ((fabs(t) < mathematicalConstants::getEpsilon1()) && (fabs(u) < mathematicalConstants::getEpsilon1()))
    phi = 0.;
  else
    phi = atan2(t,u);

  ac4C = new eulerian_angleConfiguration4C;
  ac4C->set2Theta(two_theta);
  ac4C->setOmega(omega);
  ac4C->setPhi(phi);
  ac4C->setChi(chi);

  return ac4C;
}

// Compute (h,k,l) from a sample of angles.
// Solve a linear system Ax = b where A is the product of the rotation matrices 
// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
void eulerian_constantOmegaMode4C::computeHKL(
  double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const
  throw (HKLException)
{
  double chi       = ((eulerian_angleConfiguration4C*)ac)->getChi();
  double phi       = ((eulerian_angleConfiguration4C*)ac)->getPhi();
  double omega     = ((eulerian_angleConfiguration4C*)ac)->getOmega();
  double two_theta = ((eulerian_angleConfiguration4C*)ac)->get2Theta();

  omega = omega - two_theta/2.; // bisector mode !

  double cos_chi       = cos(chi);
  double cos_phi       = cos(phi);
  double sin_chi       = sin(chi);
  double sin_phi       = sin(phi);
  double cos_omega     = cos(omega);
  double sin_omega     = sin(omega);

  smatrix OMEGA;
  smatrix CHI;
  smatrix PHI;

  // Matrix Omega
  //  | cos_omega  sin_omega  0. |
  //  |-sin_omega  cos_omega  0. |
  //  |    0.         0.      1. |
  OMEGA.set(
     cos_omega, sin_omega, 0.,
    -sin_omega, cos_omega, 0.,
        0.,        0.,     1.);

  // Matrix Chi
  //  |  cos_chi  0.  sin_chi |
  //  |     0.    1.     0.   |
  //  | -sin_chi  0.  cos_chi |
  CHI.set(
    cos_chi, 0., sin_chi,
       0.,   1.,    0.,
   -sin_chi, 0., cos_chi);

  // Matrix Phi
  //  |  cos_phi   sin_phi  0. |
  //  | -sin_phi   cos_phi  0. |
  //  |     0.        0.    1. |
  PHI.set(
     cos_phi, sin_phi, 0.,
    -sin_phi, cos_phi, 0.,
        0.,      0.,   1.);

  smatrix A(OMEGA);
  A.multiplyOnTheRight(CHI);
  A.multiplyOnTheRight(PHI);
  A.multiplyOnTheRight(UB);


  double det1 = A.get(1,1)*(A.get(2,2)*A.get(3,3)-A.get(3,2)*A.get(2,3));
  double det2 =-A.get(1,2)*(A.get(2,1)*A.get(3,3)-A.get(3,1)*A.get(2,3));
  double det3 = A.get(1,3)*(A.get(2,1)*A.get(3,2)-A.get(3,1)*A.get(2,2));
  double det = det1 + det2 + det3;

  if (fabs(det) < mathematicalConstants::getEpsilon1())
    throw HKLException(
      "det(A) is null",
      "A = OME*CHI*PHI*U*B check if one of these matrices is null",
      "eulerian_constantOmegaMode4C::computeHKL()");

  double sin_theta = sin(two_theta*0.5);
  // We have to be consistent with the conventions previously defined 
  // when we chose tau. q = 2tau * sin(theta) / lambda.
  double q = (2. * sin_theta * physicalConstants::getTau()) / lambda;

  h = q * (A.get(2,2)*A.get(3,3)-A.get(3,2)*A.get(2,3)) / det;

  k =-q * (A.get(2,1)*A.get(3,3)-A.get(3,1)*A.get(2,3)) / det;

  l = q * (A.get(2,1)*A.get(3,2)-A.get(3,1)*A.get(2,2)) / det;
}

void eulerian_constantOmegaMode4C::printOnScreen() const
{}
