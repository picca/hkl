#include "angleconfig.h"
#include <iostream>

double kappa_angleConfiguration4C::m_phiInf = 0.;
double kappa_angleConfiguration4C::m_phiSup = 0.;
double kappa_angleConfiguration4C::m_omegaInf = 0.;
double kappa_angleConfiguration4C::m_omegaSup = 0.;
double kappa_angleConfiguration4C::m_kappaInf = 0.;
double kappa_angleConfiguration4C::m_kappaSup = 0.;
double kappa_angleConfiguration4C::m_2thetaInf = 0.;
double kappa_angleConfiguration4C::m_2thetaSup = 0.;


// Default constructor which sets the four angles to to zero.
kappa_angleConfiguration4C::kappa_angleConfiguration4C()
{
  m_omega = 0.;
  m_kappa = 0.;
  m_phi = 0.;
  m_2theta = 0.;
  // The intervals associated to the angles.
  m_omegaInf = 1.;
  m_omegaSup = -1.;
  m_kappaInf = 1.;
  m_kappaSup = -1.;
  m_phiInf = 1.;
  m_phiSup = -1.;
  m_2thetaInf = 1.;
  m_2thetaSup = -1.;
}

  // Constructor with an already made configuration.
kappa_angleConfiguration4C::kappa_angleConfiguration4C(
    double omega, double kappa, double phi, double two_theta)
{
  m_omega = omega;
  m_kappa = kappa;
  m_phi = phi;
  m_2theta = two_theta;
  // The intervals associated to the angles. Set inconsistent
  // values to force revisiting them.
  m_omegaInf = 1.;
  m_omegaSup = -1.;
  m_kappaInf = 1.;
  m_kappaSup = -1.;
  m_phiInf = 1.;
  m_phiSup = -1.;
  m_2thetaInf = 1.;
  m_2thetaSup = -1.;
}

// Constructor with an already made configuration
// and the angle intervals.
kappa_angleConfiguration4C::kappa_angleConfiguration4C(
    double omega, double kappa, double phi, double two_theta,
    double oi, double os, double ki, double ks,
    double pi, double ps, double ti, double ts)
{
  m_omega = omega;
  m_kappa = kappa;
  m_phi = phi;
  m_2theta = two_theta;
  // The intervals associated to the angles.
  m_omegaInf = oi;
  m_omegaSup = os;
  m_kappaInf = ki;
  m_kappaSup = ks;
  m_phiInf = pi;
  m_phiSup = ps;
  m_2thetaInf = ti;
  m_2thetaSup = ts;
}

angleConfiguration* kappa_angleConfiguration4C::makeCopy() const
{
  kappa_angleConfiguration4C* kap4C = new
    kappa_angleConfiguration4C(
    m_omega, m_kappa, m_phi, m_2theta,
    m_omegaInf, m_omegaSup, m_kappaInf, m_kappaSup,
    m_phiInf, m_phiSup, m_2thetaInf, m_2thetaSup);

  return kap4C;
}

void kappa_angleConfiguration4C::printOnScreen()
{
  std::cout << std::endl << "CLASS kappa_angleConfiguration4C";
  std::cout << std::endl
    << "Omega = " << m_omega << '\t'
    << "Kappa = " << m_kappa << '\t'
    << "Phi = " << m_phi << '\t'
    << "2Theta = " << m_2theta << std::endl;
}

void kappa_angleConfiguration4C::printStaticOnScreen()
{
  std::cout << std::endl << "CLASS kappa_angleConfiguration4C static variables";
  std::cout << std::endl
    << "[OmegaInf = " << m_omegaInf << '\t'
    << " OmegaSup = " << m_omegaSup << "]" << '\t'
    << "[KappaInf = " << m_kappaInf << '\t'
    << " KappaSup = " << m_kappaSup << "]" << '\t'
    << "[PhiInf = " << m_phiInf << '\t'
    << " PhiSup = " << m_phiSup << "]" << '\t'
    << "[2ThetaInf = " << m_2thetaInf << '\t'
    << " 2ThetaSup = " << m_2thetaSup << "]" << std::endl;
}
