//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/eulerian_angleconfiguration4C.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class eulerian_angleconfiguration4C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: delos $

//

// $Revision: 1.5 $

//

// $Log: eulerian_angleconfiguration4C.cpp,v $
// Revision 1.5  2005/01/27 09:23:53  delos
// Commentaires pour CVS en tete des fichiers
//

//

//

// copyleft :       Synchrotron SOLEIL

//                  L'Orme des Merisiers

//                  Saint-Aubin - BP 48

//                  91192 GIF-sur-YVETTE CEDEX

//

//-======================================================================
#include "angleconfig.h"
#include <iostream>

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
  // The intervals associated to the angles. Set inconsistent values to force revisiting them.
  m_omegaInf = 1.;
  m_omegaSup = -1.;
  m_chiInf = 1.;
  m_chiSup = -1.;
  m_phiInf = 1.;
  m_phiSup = -1.;
  m_2thetaInf = 1.;
  m_2thetaSup = -1.;
}

// Constructor with an already made configuration and the angle intervals.
eulerian_angleConfiguration4C::eulerian_angleConfiguration4C(
    double omega, double chi, double phi, double two_theta,
    double omega_inf, double omega_sup, double chi_inf, double chi_sup,
    double phi_inf, double phi_sup, double theta_inf, double theta_sup):
    angleConfiguration()
{
  m_omega = omega;
  m_chi = chi;
  m_phi = phi;
  m_2theta = two_theta;
  // The intervals associated to the angles.
  m_omegaInf = omega_inf;
  m_omegaSup = omega_sup;
  m_chiInf = chi_inf;
  m_chiSup = chi_sup;
  m_phiInf = phi_inf;
  m_phiSup = phi_sup;
  m_2thetaInf = theta_inf;
  m_2thetaSup = theta_sup;
}

angleConfiguration* eulerian_angleConfiguration4C::makeCopy() const
{
  eulerian_angleConfiguration4C* eul4C = new eulerian_angleConfiguration4C(
    m_omega, m_chi, m_phi, m_2theta,
    m_omegaInf, m_omegaSup, m_chiInf, m_chiSup,
    m_phiInf, m_phiSup, m_2thetaInf, m_2thetaSup);

  return eul4C;
}

void eulerian_angleConfiguration4C::printOnScreen()
{
  std::cout.precision(20);
  std::cout << std::endl << "CLASS eulerian_angleConfiguration4C";
  std::cout << std::endl
    << "Omega = " << m_omega << '\t'
    << "Chi = " << m_chi << '\t'
    << "Phi = " << m_phi << '\t'
    << "2Theta = " << m_2theta << std::endl;
}

void eulerian_angleConfiguration4C::printDegreesOnScreen()
{
  double RadToDeg = 180. / 3.141592654;
  std::cout << std::endl << "CLASS eulerian_angleConfiguration4C in degrees";
  std::cout << std::endl
    << "Omega = " << m_omega * RadToDeg << '\t'
    << "Chi = " << m_chi * RadToDeg << '\t'
    << "Phi = " << m_phi * RadToDeg << '\t'
    << "2Theta = " << m_2theta * RadToDeg << std::endl;
}

void eulerian_angleConfiguration4C::printStaticOnScreen()
{
  std::cout << std::endl << "CLASS eulerian_angleConfiguration4C static variables";
  std::cout << std::endl
    << "[OmegaInf = " << m_omegaInf << '\t'
    << " OmegaSup = " << m_omegaSup << "]" << '\t'
    << "[ChiInf = " << m_chiInf << '\t'
    << " ChiSup = " << m_chiSup << "]" << '\t'
    << "[PhiInf = " << m_phiInf << '\t'
    << " PhiSup = " << m_phiSup << "]" << '\t'
    << "[2ThetaInf = " << m_2thetaInf << '\t'
    << " 2ThetaSup = " << m_2thetaSup << "]" << std::endl;
}
