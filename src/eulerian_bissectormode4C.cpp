#include <math.h>
#include "mode.h"
#include "angleconfig.h"

#define EPSILON 1e-10

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
// hphi1 = 
//    q(-sin(omega)*sin(phi)+cos(omega)*cos(chi)*cos(phi))
// hphi2 = 
//    q( sin(omega)*cos(phi)+cos(omega)*cos(chi)*sin(phi))
// hphi3 = q*cos(omega)*sin(chi)
//
// If omega is constant :
// chi = arcsin(hphi3 / q*cos(omega))
// sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
// cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
// D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) +
//      sin(omega)*sin(omega)]
angleConfiguration*
  eulerian_bissectorMode4C::computeAngles(
  int h, int k, int l, 
  const smatrix& UB,
  double lembda) const
{
  eulerian_angleConfiguration4C* ac4C =
    new eulerian_angleConfiguration4C;
  // h(theta) = R.hphi
  double two_theta;
  double omega;
  double chi;
  double phi;
  double sin_theta;
  double hphi_length;
  svector hphi_unitVector;
  svector hphi( (double)h, (double)k, (double)l);

  hphi.multiplyOnTheLeft(UB);
  hphi.unitVector(hphi_unitVector, hphi_length);

  // sin(theta) = || q || * lembda * 0.5
  sin_theta = hphi_length * lembda * 0.5;

  //if (sin_theta > 1.)

  two_theta = 2. * asin(sin_theta);

  omega = asin(sin_theta);
  double omega_prime = 0.;
  omega = 0.;


  double so = sin(omega);
  double co = cos(omega);
  //double so = sin(omega_prime);
  //double co = cos(omega_prime);
  // SPEC limit case.
  if (fabs(co) < EPSILON)
  {
    phi = atan2(-so * hphi.get_X(), so * hphi.get_Y());
    ac4C->setOmega(omega);
    ac4C->setPhi(phi);
    ac4C->setChi(0.);
    return ac4C;
  }
  double sx = hphi.get_Z() / (hphi_length * co);
  // SPEC : without hphi_length
  //double sx = hphi.get_Z() / (co);

  chi = asin(sx);
  // asin() returns values between -PI/2. and PI/2.
  // hphi 1st component sign tells whether or not
  // chi belongs to the other half of the circle i.e.
  // between PI/2. and 3PI/2.
  if (hphi.get_X() < 0.)
    chi = 3.141592654 - chi;

  double t = -so*hphi.get_X() + co*cos(chi)*hphi.get_Y();
  double u =  so*hphi.get_Y() + co*cos(chi)*hphi.get_X();
  phi = atan2(t,u);

  // RAFIN.
  //if (thetaIsNegative == TRUE)
  //{
    //chi = chi * (-1);
    //phi = 3.141592654 - phi;
    //omega = omega * (-1);
    //two_theta = two_theta * (-1);
  //}

  ac4C->set2Theta(two_theta);
  ac4C->setOmega(omega);
  ac4C->setPhi(phi);
  ac4C->setChi(chi);

  return ac4C;
}

void eulerian_bissectorMode4C::printOnScreen() const
{}
