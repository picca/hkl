// Cristal class functions to define the different lattices.

#include "cristal.h"
#include <iostream.h>
#include <math.h>

cristal::cristal(
    double alpha1, double alpha2, double alpha3,
    double beta1, double beta2, double beta3,
    double a1, double a2, double a3, 
    double b1, double b2, double b3)
{
  m_alpha1 = alpha1;
  m_alpha2 = alpha2;
  m_alpha3 = alpha3;
  m_beta1 = beta1;
  m_beta2 = beta2;
  m_beta3 = beta3;
  m_a1 = a1;
  m_a2 = a2;
  m_a3 = a3;
  m_b1 = b1;
  m_b2 = b2;
  m_b3 = b3;
}

cristal::cristal(cristal& C)
{
  m_alpha1 = C.m_alpha1;
  m_alpha2 = C.m_alpha2;
  m_alpha3 = C.m_alpha3;
  m_beta1 = C.m_beta1;
  m_beta2 = C.m_beta2;
  m_beta3 = C.m_beta3;
  m_a1 = C.m_a1;
  m_a2 = C.m_a2;
  m_a3 = C.m_a3;
  m_b1 = C.m_b1;
  m_b2 = C.m_b2;
  m_b3 = C.m_b3;
}

void cristal::computeB()
{
  m_B.set(m_b1, m_b2 * cos(m_beta3), m_b3 * cos(m_beta2),
          0., m_b2 * sin(m_beta3), -m_b3 * sin(m_beta2) * cos(m_alpha1),
          0., 0., 1. / m_a3);
}

void cristal::printOnScreen() const
{
  cout << endl << "CLASS cristal";
  cout << endl << "Direct lattice";
  cout << endl << 
    "alpha1=" << m_alpha1 << '\t' <<
    "alpha2=" << m_alpha2 << '\t' <<
    "alpha3=" << m_alpha3 << endl;
  cout << endl << 
    "a1=" << m_a1 << '\t' <<
    "a2=" << m_a2 << '\t' <<
    "a3=" << m_a3 << endl;
  cout << endl << "Reciprocal lattice";
  cout << endl << 
    "beta1=" << m_beta1 << '\t' <<
    "beta2=" << m_beta2 << '\t' <<
    "beta3=" << m_beta3 << endl;
  cout << endl << 
    "b1=" << m_b1 << '\t' <<
    "b2=" << m_b2 << '\t' <<
    "b3=" << m_b3 << endl;
  m_B.printOnScreen();
}