//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/cristal.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class cristal

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.12 $

//

// $Log: cristal.h,v $
// Revision 1.12  2005/02/10 13:20:07  picca
// documentation
//
// Revision 1.11  2005/01/27 09:23:53  delos
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
#ifndef _CRISTAL_H_
#define _CRISTAL_H_

#include "svecmat.h"

/**
 * \brief Class which store the cristals parameters
 *
 * Class cristal to store direct and reciprocal lattice 
 * parameters and the matrix to move from the reciprocal
 * lattice to the cristal cartesian system.
 * References :
 *
 * William R. Busing and Henri A. Levy "Angle calculation 
 * for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
 * <A HREF="http://journals.iucr.org/index.html"> Acta
 * Cryst.</A>, <B>22</B>, 457-464.
 *
 * A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
 * By Finite and Imperfect Crystals"
 * (1962) John Wiley & Sons Inc., 14-17.
 */
class cristal
{
private:
  double m_alpha1; //!< The first angle direct lattice.
  double m_alpha2; //!< The second angle direct lattice.
  double m_alpha3; //!< The third angle direct lattice.
  double m_beta1; //!< The first angle reciprocal lattice.
  double m_beta2; //!<  The second angle reciprocal lattice.
  double m_beta3; //!<  The third angle reciprocal lattice.
  double m_a1; //!< The first length direct lattice.
  double m_a2; //!< The second length direct lattice.
  double m_a3; //!<  The third length direct lattice.
  double m_b1; //!<  The first length reciprocal lattice.
  double m_b2; //!<  The second length reciprocal lattice.
  double m_b3; //!<  The third length reciprocal lattice.
  
  /**
   * \brief The crystal matrix.
   *
   * This matrix takes us from the reciprocal lattice
   * to the crystal cartesian system.
   */  
  smatrix m_B;

  /**
   * \brief Calculate the reciprocal lattice from the direct one.
   *
   * This method computes the reciprocal lattice \f$(\vec{a}^*, \vec{b}^*, \vec{c}^*)\f$
   * from the direct one \f$(\vec{a}, \vec{b}, \vec{c})\f$.
   * \f[
   *  \begin{aligned}
   *    \vec{a}^* &= \tau\frac{\vec{b}\times\vec{c}}{\vec{a}.(\vec{b}\times\vec{c})} \\
   *    \vec{b}^* &= \tau\frac{\vec{c}\times\vec{a}}{\vec{b}.(\vec{c}\times\vec{a})} \\
   *    \vec{c}^* &= \tau\frac{\vec{a}\times\vec{b}}{\vec{c}.(\vec{a}\times\vec{b})}
   *  \end{aligned}
   * \f]
   * where \f$\tau\f$ is physicalConstants::m_tau
   */
  void computeReciprocalLattice();

  /**
   * \brief The main function to compute the matrix B.
   *
   * This method computes the B matrix from the cristal parameters.
   * It is the transformation matrix from the reciprocal base to an orthonormal
   * one.
   * \f[
   *  B = \left(
   *    \begin{matrix}
   *      b_1 & b_2\cos\beta_3  & b_3\cos\beta_2 \\
   *      0   & b_2\sin\beta_3  & -b_3\sin\beta_2\cos\alpha_1 \\
   *      0   & 0               & 1/a_3
   *    \end{matrix}
   *  \right)
   * \f]
   */
  void computeB();

public:

  /**
   * \brief Constructor
   * @param alpha1  The direct space first angle.
   * @param alpha2 The direct space second angle.
   * @param alpha3 The direct space third angle.
   * @param beta1 The reciprocal space first angle.
   * @param beta2 The reciprocal space second angle.
   * @param beta3 The reciprocal space third angle.
   * @param a1 The direct space first length.
   * @param a2 The direct space second length.
   * @param a3 The direct space third length.
   * @param b1 The reciprocal space first length.
   * @param b2 The reciprocal space second length.
   * @param b3 The reciprocal space third length.
   *
   * Constructor to fill the class with data from both 
   * the direct and reciprocal lattice. Length units for
   * a1, a2, a3, b1, b2, b3 have to be consistent with the
   * wave length defined in the class source.
   */
  cristal(
    double alpha1, double alpha2, double alpha3,
    double beta1, double beta2, double beta3,
    double a1, double a2, double a3, 
    double b1, double b2, double b3);

  /**
   * \brief Constructor
   * @param alpha1 alpha1 The direct space first angle.
   * @param alpha2 alpha2 The direct space second angle.
   * @param alpha3 alpha3 The direct space third angle.
   * @param a1 a1 The direct space first length.
   * @param a2 a2 The direct space second length.
   * @param a3 a3 The direct space third length.
   * 
   * Constructor to fill the class with data from the
   * direct lattice and compute the reciprocal  parameters
   * with computeReciprocalLattice(), then call computeB().
   * Length units for a1, a2, a3 have to be consistent with 
   * the wave length defined in the class source.
   */
  cristal(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3);

  /**
   * \brief Copy constructor
   * @param C The crystal we want to copy.
   *
   * This constructor creates a new crystal from the C crystal.
   */
  cristal(const cristal &C);

  double getAlpha1() const {return m_alpha1;} //!< get the m_alpha1 parameter
  double getAlpha2() const {return m_alpha2;} //!< get the m_alpha2 parameter
  double getAlpha3() const {return m_alpha3;} //!< get the m_alpha3 parameter
  double getBeta1() const {return m_beta1;} //!< get the m_beta1 parameter
  double getBeta2() const {return m_beta2;} //!< get the m_beta2 parameter
  double getBeta3() const {return m_beta3;} //!< get the m_beta3 parameter
  double get_a1() const {return m_a1;} //!< get the m_a1 parameter
  double get_a2() const {return m_a2;} //!< get the m_a2 parameter
  double get_a3() const {return m_a3;} //!< get the m_a3 parameter
  double get_b1() const {return m_b1;} //!< get the m_b1 parameter
  double get_b2() const {return m_b2;} //!< get the m_b2 parameter
  double get_b3() const {return m_b3;} //!< get the m_b3 parameter
  smatrix get_B() const {return m_B;} //!< get the m_B parameter
  
  /**
   * \brief Reset the fields with new values and recompute the reciprocal lattice and B.
   * @param alpha1 The direct space first angle.
   * @param alpha2 The direct space second angle.
   * @param alpha3 The direct space third angle.
   * @param a1 The direct space first length.
   * @param a2 The direct space second length.
   * @param a3 The direct space third length.
   *
   * Fill the class with data from the direct lattice  *  
   * and compute the reciprocal  parameters with
   * computeReciprocalLattice(), then call computeB().  
   * Length units for a1,a2,a3 have to be consistent with
   * the wave length defined in the class source.
   */
   void set(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3);

  /**
   * \brief Reset the fields with new values from an other cristal.
   * @param C The crystal we want to copy.
   *
   * Fille the class with data from the cristal C
   */
  void set(const cristal& C);
  
  /**
   * \brief Check  if the matrices B are the same in both crystals.
   * @param B The matrix of the cristal to test
   * @return 0 if everything is OK, -1 otherwise.
   *
   * Check  if the matrices B are the same in both crystals.
   */
  int check_cristal(const smatrix& B) const;

  /**
   * \brief Print on screen a cristal
   *
   * this method print on the stdout all the parameters of the cristal
   */
  void printOnScreen() const;

  /*
  * Test six different cristals (cubic, orthorombic,
  * hexagonal, triclinic) to make sure the computations
  * are OK.
  * \return 0 if everything's fine, otherwise return
  * the number of the cristal whose reciprocal lattice
  * or matrix is wrong.
  */
  static int test_cristals();


};

#endif // _CRISTAL_H_
