#ifndef _LATTICE_H_
#define _LATTICE_H_

#include <math.h>
#include <iostream>
#include "svecmat.h"
#include "object.h"

class Lattice
{
  public:
  
  Lattice();
  Lattice(double a, double b, double c, double alpha, double beta, double gamma);
  Lattice(Lattice const & lattice);
  
  bool operator == (Lattice const & cP) const;
  
  virtual ~Lattice();
  
  double get_a() const {return m_a;} //!< get the m_a parameter
  double get_b() const {return m_b;} //!< get the m_b parameter
  double get_c() const {return m_c;} //!< get the m_c parameter
  double get_alpha() const {return m_alpha;} //!< get the m_alpha angle
  double get_beta() const {return m_beta;} //!< get the m_beta angle
  double get_gamma() const {return m_gamma;} //!< get the m_gamma angle
 
  void set_a(double a) {m_a = a;} //!< set the m_a parameter.
  void set_b(double b) {m_b = b;} //!< set the m_b parameter.
  void set_c(double c) {m_c = c;} //!< set the m_c parameter.  
  void set_alpha(double alpha) {m_alpha = alpha;} //!< set the m_alpha angle.
  void set_beta(double beta) {m_beta = beta;} //!< set the m_beta angle.
  void set_gamma(double gamma) {m_gamma = gamma;} //!< set the m_gamma angle.
  
  void get(double * a, double * b, double * c,
           double * alpha, double * beta, double * gamma) const;
  void set(double a, double b, double c, double alpha, double beta, double gamma);
  Lattice computeReciprocalLattice() const throw (HKLException);
  
  private:
    
  double m_a; //!< The length of \f$\vec{a}\f$.
  double m_b; //!< The length of \f$\vec{b}\f$.
  double m_c; //!< The length of \f$\vec{c}\f$.
  double m_alpha; //!< The angle between \f$\vec{b},\vec{c}\f$.
  double m_beta; //!< The angle between \f$\vec{c},\vec{a}\f$.
  double m_gamma; //!< The angle between \f$\vec{a},\vec{b}\f$.
};

std::ostream& operator << (std::ostream& flux, Lattice const & l);

#endif	//_LATTICE_H_
