#include "angleconfig.h"
#include <iostream.h>

double eulerian_angleConfiguration4C::m_chiInf = 0.;
double eulerian_angleConfiguration4C::m_chiSup = 0.;
double eulerian_angleConfiguration4C::m_phiInf = 0.;
double eulerian_angleConfiguration4C::m_phiSup = 0.;
double eulerian_angleConfiguration4C::m_omegaInf = 0.;
double eulerian_angleConfiguration4C::m_omegaSup = 0.;
double eulerian_angleConfiguration4C::m_2thetaInf = 0.;
double eulerian_angleConfiguration4C::m_2thetaSup = 0.;


// Default constructor which sets the four angles to to zero.
eulerian_angleConfiguration4C::eulerian_angleConfiguration4C():
angleConfiguration()
{
  m_omega = 0.;
  m_chi = 0.;
  m_phi = 0.;
  m_2theta = 0.;
  // The intervals associated to the angles.
  m_omegaInf = 1.;
  m_omegaSup = -1.;
  m_chiInf = 1.;
  m_chiSup = -1.;
  m_phiInf = 1.;
  m_phiSup = -1.;
  m_2thetaInf = 1.;
  m_2thetaSup = -1.;
}

  // Constructor with an already made configuration.
eulerian_angleConfiguration4C::eulerian_angleConfiguration4C(
    double omega, double chi, double phi, double two_theta):
    angleConfiguration()
{
  m_omega = omega;
  m_chi = chi;
  m_phi = phi;
  m_2theta = two_theta;
  // The intervals associated to the angles. Set inconsistent
  // values to force revisiting them.
  m_omegaInf = 1.;
  m_omegaSup = -1.;
  m_chiInf = 1.;
  m_chiSup = -1.;
  m_phiInf = 1.;
  m_phiSup = -1.;
  m_2thetaInf = 1.;
  m_2thetaSup = -1.;
}

// Constructor with an already made configuration
// and the angle intervals.
eulerian_angleConfiguration4C::eulerian_angleConfiguration4C(
    double omega, double chi, double phi, double two_theta,
    double oi, double os, double ci, double cs,
    double pi, double ps, double ti, double ts):
    angleConfiguration()
{
  m_omega = omega;
  m_chi = chi;
  m_phi = phi;
  m_2theta = two_theta;
  // The intervals associated to the angles.
  m_omegaInf = oi;
  m_omegaSup = os;
  m_chiInf = ci;
  m_chiSup = cs;
  m_phiInf = pi;
  m_phiSup = ps;
  m_2thetaInf = ti;
  m_2thetaSup = ts;
}

angleConfiguration* eulerian_angleConfiguration4C::makeCopy() const
{
  eulerian_angleConfiguration4C* eul4C = new 
    eulerian_angleConfiguration4C(
    m_omega, m_chi, m_phi, m_2theta,
    m_omegaInf, m_omegaSup, m_chiInf, m_chiSup,
    m_phiInf, m_phiSup, m_2thetaInf, m_2thetaSup);

  return eul4C;
}

void eulerian_angleConfiguration4C::printOnScreen()
{
  cout << endl << "CLASS eulerian_angleConfiguration4C";
  cout << endl
    << "Omega = " << m_omega << '\t'
    << "Chi = " << m_chi << '\t'
    << "Phi = " << m_phi << '\t'
    << "2Theta = " << m_2theta << endl;
}

void eulerian_angleConfiguration4C::printStaticOnScreen()
{
  cout << endl << "CLASS eulerian_angleConfiguration4C static variables";
  cout << endl
    << "[OmegaInf = " << m_omegaInf << '\t'
    << " OmegaSup = " << m_omegaSup << "]" << '\t'
    << "[ChiInf = " << m_chiInf << '\t'
    << " ChiSup = " << m_chiSup << "]" << '\t'
    << "[PhiInf = " << m_phiInf << '\t'
    << " PhiSup = " << m_phiSup << "]" << '\t'
    << "[2ThetaInf = " << m_2thetaInf << '\t'
    << " 2ThetaSup = " << m_2thetaSup << "]" << endl;
}
