#include "angleconfig.h"
#include "constants.h"
#include <iostream.h>

double eulerian_angleConfiguration6C::m_muInf = 0.;
double eulerian_angleConfiguration6C::m_muSup = 0.;
double eulerian_angleConfiguration6C::m_nuInf = 0.;
double eulerian_angleConfiguration6C::m_nuSup = 0.;


// Default constructor which sets the four angles to to zero.
eulerian_angleConfiguration6C::eulerian_angleConfiguration6C():
eulerian_angleConfiguration4C()
{
  m_mu = 0.;
  m_nu = 0.;
  // The intervals associated to the angles.
  m_muInf = 1.;
  m_muSup = -1.;
  m_nuInf = 1.;
  m_nuSup = -1.;
}

  // Constructor with an already made configuration.
eulerian_angleConfiguration6C::eulerian_angleConfiguration6C(
    double mu, double eta, double chi, double phi, double nu, double delta):
    eulerian_angleConfiguration4C(eta, chi, phi, delta)
{
  m_mu = mu;
  m_nu = nu;

  m_omega = eta;    // already done but why not insisting ?
  m_2theta = delta; // same thing as before...

  // The intervals associated to the angles. Set inconsistent values to force revisiting them.
  m_nuInf = 1.;
  m_muInf = 1.;
  m_nuSup = -1.;
  m_muSup = -1.;
}

// Constructor with an already made configuration
// and the angle intervals.
eulerian_angleConfiguration6C::eulerian_angleConfiguration6C(
    double mu, double eta, double chi, double phi, double nu, double delta,
    double mi, double ms, double ei, double es, double ci, double cs,
    double pi, double ps, double ni, double ns, double di, double ds):
    eulerian_angleConfiguration4C(eta, chi, phi, delta, ei, es, ci, cs, pi, ps, di, ds)
{
  m_mu = mu;
  m_nu = nu;

  m_omega = eta;    // already done but why not insisting ?
  m_2theta = delta; // same thing as before...

  // The intervals associated to the angles.
  m_nuInf = ni;
  m_muInf = mi;
  m_nuSup = ns;
  m_muSup = ms;
}

angleConfiguration* eulerian_angleConfiguration6C::makeCopy() const
{
  eulerian_angleConfiguration6C* eul6C = new 
    eulerian_angleConfiguration6C(
    m_mu, m_omega, m_chi, m_phi, m_nu, m_2theta,
    m_muInf, m_muSup, m_omegaInf, m_omegaSup, m_chiInf, m_chiSup,
    m_phiInf, m_phiSup, m_nuInf, m_nuSup, m_2thetaInf, m_2thetaSup);

  return eul6C;
}

void eulerian_angleConfiguration6C::printOnScreen()
{
  cout.precision(20);
  cout << endl << "CLASS eulerian_angleConfiguration6C";
  cout << endl
    << "Mu = " << m_mu << '\t'
    << "Eta = " << m_omega << '\t'
    << "Chi = " << m_chi << '\t'
    << "Phi = " << m_phi << '\t'
    << "Nu = " << m_nu << '\t'
    << "Delta = " << m_2theta << endl;
}

void eulerian_angleConfiguration6C::printDegreesOnScreen()
{
  double RadToDeg = mathematicalConstants::convertAnglesToDegrees();
  cout << endl << "CLASS eulerian_angleConfiguration6C in degrees";
  cout << endl
    << "Mu = " << m_mu * RadToDeg << '\t'
    << "Eta = " << m_omega * RadToDeg << '\t'
    << "Chi = " << m_chi * RadToDeg << '\t'
    << "Phi = " << m_phi * RadToDeg << '\t'
    << "Nu = " << m_nu * RadToDeg << '\t'
    << "Delta = " << m_2theta * RadToDeg << endl;
}

void eulerian_angleConfiguration6C::printStaticOnScreen()
{
  cout << endl << "CLASS eulerian_angleConfiguration6C static variables";
  cout << endl
    << "[MuInf  = " << m_muInf << '\t'
    << " MuSup  = " << m_muSup << "]" << '\t'
    << "[EtaInf = " << m_omegaInf << '\t'
    << " EtaSup = " << m_omegaSup << "]" << '\t'
    << "[ChiInf = " << m_chiInf << '\t'
    << " ChiSup = " << m_chiSup << "]" << '\t'
    << "[PhiInf = " << m_phiInf << '\t'
    << " PhiSup = " << m_phiSup << "]" << '\t'
    << "[NuInf  = " << m_nuInf << '\t'
    << " NuSup  = " << m_nuSup << "]" << '\t'
    << "[DeltaInf = " << m_2thetaInf << '\t'
    << " DeltaSup = " << m_2thetaSup << "]" << endl;
}

int eulerian_angleConfiguration6C::test_angle6C()
{
  eulerian_angleConfiguration6C eul6C_1;
  eul6C_1.printOnScreen();
  eul6C_1.printDegreesOnScreen();
  eul6C_1.printStaticOnScreen();
  cout << endl << "**********" << endl;

  eulerian_angleConfiguration6C eul6C_2(1.,2.,3.141592654,4.,5.,6.);
  eul6C_2.printOnScreen();
  eul6C_2.printDegreesOnScreen();
  eul6C_2.printStaticOnScreen();
  cout << endl << "**********" << endl;

  eulerian_angleConfiguration6C eul6C_3(
    -1., -2., -3., -4., -5., -6.,
    -10., 10., -20., 20., -30., 30.,
    -40., 40., -50., 50., -60., 60.);
  eul6C_3.printOnScreen();
  eul6C_3.printDegreesOnScreen();
  eul6C_3.printStaticOnScreen();
  cout << endl << "**********" << endl;

  eulerian_angleConfiguration6C* eul6C_4 = (eulerian_angleConfiguration6C*)eul6C_3.makeCopy();
  eul6C_4->setMu(1.);
  eul6C_4->setEta(2.);
  eul6C_4->setChi(3.);
  eul6C_4->setPhi(4.);
  eul6C_4->setNu(5.);
  eul6C_4->setDelta(6.);
  eul6C_4->setMuInf(10.);
  eul6C_4->setEtaInf(20.);
  eul6C_4->setChiInf(30.);
  eul6C_4->setPhiInf(40.);
  eul6C_4->setNuInf(50.);
  eul6C_4->setDeltaInf(60.);
  eul6C_4->setMuSup(100.);
  eul6C_4->setEtaSup(200.);
  eul6C_4->setChiSup(300.);
  eul6C_4->setPhiSup(400.);
  eul6C_4->setNuSup(500.);
  eul6C_4->setDeltaSup(600.);
  eul6C_4->printOnScreen();
  eul6C_4->printDegreesOnScreen();
  eul6C_4->printStaticOnScreen();


  delete eul6C_4;

  return 0;
}
