/// Class cristal to store direct and reciprocal lattice 
/// parameters and the matrix to move from the reciprocal
/// lattice to the cristal cartesian system.
///
/// References :
///
/// William R. Busing and Henri A. Levy "Angle calculation 
/// for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
///
/// A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
/// By Finite and Imperfect Crystals"
/// (1962) John Wiley & Sons Inc., 14-17.
///
#ifndef CRISTAL
#define CRISTAL


#include "svecmat.h"

class cristal
{
private:
  /// The first angle direct lattice.
  double m_alpha1;
  /// The second angle direct lattice.
  double m_alpha2;
  /// The third angle direct lattice.
  double m_alpha3;
  /// The first angle reciprocal lattice.
  double m_beta1;
  /// The second angle reciprocal lattice.
  double m_beta2;
  /// The third angle reciprocal lattice.
  double m_beta3;
  /// The first length direct lattice.
  double m_a1;
  /// The second length direct lattice.
  double m_a2;
  /// The third length direct lattice.
  double m_a3;
  /// The first length reciprocal lattice.
  double m_b1;
  /// The second length reciprocal lattice.
  double m_b2;
  /// The third length reciprocal lattice.
  double m_b3;
  /// This matrix takes us from the reciprocal lattice 
  /// to the crystal cartesian system.
  /// \brief The crystal matrix.
  smatrix m_B;

  /// Calculate the reciprocal lattice from the direct one.
  void computeReciprocalLattice();

  /// The main function to compute the matrix B.
  void computeB();

public:
  /// Constructor to fill the class with data from both 
  /// the direct and reciprocal lattice. Length units for
  /// a1,a2,a3,b1,b2,b3 have to be consistent with the
  /// wave length defined in the class source.
  cristal(
    double alpha1, double alpha2, double alpha3,
    double beta1, double beta2, double beta3,
    double a1, double a2, double a3, 
    double b1, double b2, double b3);

  /// Constructor to fill the class with data from the
  /// direct lattice and compute the reciprocal  parameters
  /// with computeReciprocalLattice(), then call computeB().
  /// Length units for a1,a2,a3 have to be consistent with 
  /// the wave length defined in the class source.
  cristal(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3);

  /// Copy constructor.
  cristal(const cristal& C);

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

  /// Fill the class with data from the direct lattice
  /// and compute the reciprocal  parameters with
  /// computeReciprocalLattice(), then call computeB().
  /// Length units for a1,a2,a3 have to be consistent with 
  /// the wave length defined in the class source.
  void set(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3);

  /// Copy a cristal.
  void set(const cristal& C);

  smatrix get_B() const {return m_B;}

  void printOnScreen() const;

  /// Test six different cristals (cubic, orthorombic,
  /// hexagonal, triclinic) to make sure the computations
  /// are OK.
  /// \return 0 if everything's fine, otherwise return
  /// the number of the cristal whose reciprocal lattice
  /// or matrix is wrong.
  static int test_cristals();

  /// Check if the matrices B are the same in both crystals.
  /// \return 0 if everything is OK, -1 otherwise.
  int check_cristal(const smatrix& B) const;

};

#endif
