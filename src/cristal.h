// Class cristal to store direct and reciprocal lattice parameters and
// the matrix to move from the reciprocal lattice to the cristal 
// cartesian system. Reference : William R. Busing and Henri A. Levy 
// "Angle calculation for 3- and 4- Circle X-ray and Neutron 
// Diffractometer" (1967) Acta Cryst., 22, 457-464
#ifndef CRISTAL
#define CRISTAL

#include "svecmat.h"

class cristal
{
private:
  // The angles between the direct lattice axes.
  double m_alpha1;
  double m_alpha2;
  double m_alpha3;
  // The angles between the reciprocal lattice axes.
  double m_beta1;
  double m_beta2;
  double m_beta3;
  // The direct lattice axes lengths.
  double m_a1;
  double m_a2;
  double m_a3;
  // The reciprocal lattice axes lengths.
  double m_b1;
  double m_b2;
  double m_b3;
  // The matrix to move from the reciprocal lattice to the cristal
  // cartesian system.
  smatrix m_B;
  // The name of the cristal.
  //char* m_cristalName;

public:
  // Main constructor to populate it with usual data.
  cristal(
    double alpha1, double alpha2, double alpha3,
    double beta1, double beta2, double beta3,
    double a1, double a2, double a3, 
    double b1, double b2, double b3);

  cristal(cristal& C);

  double getAlpha1() const {return m_alpha1;}
  double getAlpha2() const {return m_alpha2;}
  double getAlpha3() const {return m_alpha3;}
  double getBeta1() const {return m_beta1;}
  double getBeta2() const {return m_beta2;}
  double getBeta3() const {return m_beta3;}
  double get_a1() const {return m_a1;}
  double get_a2() const {return m_a2;}
  double get_a3() const {return m_a3;}
  double get_b1() const {return m_b1;}
  double get_b2() const {return m_b2;}
  double get_b3() const {return m_b3;}

  smatrix get_B() const {return m_B;}

  // The main function to compute matrix B.
  void computeB();

  void printOnScreen() const;

};

#endif