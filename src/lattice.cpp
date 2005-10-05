//
// File: Lattice.cc
// Created by: User <Email>
// Created on: Fri Apr  1 16:14:31 2005
//

#include "lattice.h"

Lattice::Lattice()
{
  double degToRad = mathematicalConstants::convertAnglesToRadians();
  
  m_a = 1.;
  m_b = 1.;
  m_c = 1.;
  m_alpha = 90. * degToRad;
  m_beta = 90. * degToRad;
  m_gamma = 90. * degToRad;
}

Lattice::Lattice(double a, double b, double c,
                 double alpha, double beta, double gamma) :
  m_a(a),
  m_b(b),
  m_c(c),
  m_alpha(alpha),
  m_beta(beta),
  m_gamma(gamma)
{
}

Lattice::Lattice(Lattice const & lattice) :
  m_a(lattice.m_a),
  m_b(lattice.m_b),
  m_c(lattice.m_c),
  m_alpha(lattice.m_alpha),
  m_beta(lattice.m_beta),
  m_gamma(lattice.m_gamma)
{
}

bool
Lattice::operator == (Lattice const & lattice) const
{
  double epsilon = mathematicalConstants::getEpsilon1();
  
  return fabs(m_a -lattice.m_a) < epsilon
    && fabs(m_b - lattice.m_b) < epsilon
    && fabs(m_c - lattice.m_c) < epsilon
    && fabs(m_alpha - lattice.m_alpha) < epsilon
    && fabs(m_beta - lattice.m_beta) < epsilon
    && fabs(m_gamma - lattice.m_gamma) < epsilon;
}

void
Lattice::get(double * a, double * b, double * c,
              double * alpha, double * beta, double * gamma) const
{
  *a = m_a;
  *b = m_b;
  *c = m_c;
  *alpha = m_alpha;
  *beta = m_beta;
  *gamma = m_gamma;
}

void
Lattice::set(double a, double b, double c,
              double alpha, double beta, double gamma)
{
  m_a = a;
  m_b = b;
  m_c = c;
  m_alpha = alpha;
  m_beta = beta;
  m_gamma = gamma;
}

// A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
// By Finite and Imperfect Crystals" (1962)
// John Wiley & Sons Inc., 14-17.
Lattice
Lattice::computeReciprocalLattice() const throw (HKLException)
{
  double V2 =  1 
    - cos(m_alpha)*cos(m_alpha) 
    - cos(m_beta)*cos(m_beta)
    - cos(m_gamma)*cos(m_gamma)
    + 2*cos(m_alpha)*cos(m_beta)*cos(m_gamma);
  if (V2 < 0.)
    throw HKLException("Problem with the lattice parameter",
                       "the 3 angles are not compatibles.",
                       "Lattice::computeReciprocalLattice");
  
  double D = sqrt( 1 
    - cos(m_alpha)*cos(m_alpha) 
    - cos(m_beta)*cos(m_beta)
    - cos(m_gamma)*cos(m_gamma)
    + 2*cos(m_alpha)*cos(m_beta)*cos(m_gamma));
 
  //std::cout << " V2: " << V2 << " " << D << std::endl;

  double m_b1 = (sin(m_alpha) / (m_a * D));
  double m_b2 = (sin(m_beta) / (m_b * D));
  double m_b3 = (sin(m_gamma) / (m_c * D));

  double cos_beta1 =
    (cos(m_beta)*cos(m_gamma) - cos(m_alpha)) /
    (sin(m_beta)*sin(m_gamma));
  double cos_beta2 = 
    (cos(m_gamma)*cos(m_alpha) - cos(m_beta)) /
    (sin(m_gamma)*sin(m_alpha));
  double cos_beta3 = 
    (cos(m_alpha)*cos(m_beta) - cos(m_gamma)) /
    (sin(m_alpha)*sin(m_beta));
  double sin_beta1 = D / (sin(m_beta) * sin(m_gamma));
  double sin_beta2 = D / (sin(m_gamma) * sin(m_alpha));
  double sin_beta3 = D / (sin(m_alpha) * sin(m_beta));

  // The constant tau 
  m_b1 = physicalConstants::getTau() * m_b1;
  m_b2 = physicalConstants::getTau() * m_b2;
  m_b3 = physicalConstants::getTau() * m_b3;

  double m_beta1 = atan2(sin_beta1, cos_beta1);
  double m_beta2 = atan2(sin_beta2, cos_beta2);
  double m_beta3 = atan2(sin_beta3, cos_beta3);
  
  return Lattice(m_b1, m_b2, m_b3, m_beta1, m_beta2, m_beta3);
}

Lattice::~Lattice()
{
}

std::ostream &
operator << (std::ostream & flux, Lattice const & l)
{
  double radToDeg = mathematicalConstants::convertAnglesToDegrees();
  
  flux << l.get_a() << " "
    << l.get_b() << " "
    << l.get_c() << " "
    << l.get_alpha()*radToDeg << " "
    << l.get_beta()*radToDeg << " "
    << l.get_gamma()*radToDeg;
  
  return flux;
}
