//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/cristal.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class cristal

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.4 $

//

// $Log: cristal.h,v $
// Revision 1.4  2005/10/20 12:48:47  picca
// * right calculation for the number of usable reflections
// close: #976 #977
//
// Revision 1.2.2.2  2005/10/20 12:40:20  picca
// * modification of AngleConfiguration::getAxesNames()
// * add Reflection::isColinear() + test functions
// * add Crystal::isEnoughReflections() + test functions
// * remove crystal::getNumberOfReflectionsForCalculation() what a silly name :)
// * close #976 #977
//
// Revision 1.2.2.1  2005/10/11 09:24:07  picca
// *fix the MSVC++6 C4786 warning using a #pragma
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.33  2005/08/29 07:53:37  picca
//   GENERAL
//     + création des namespace:
//         hkl
//         hkl::angleConfiguration
//         hkl::angleConfiguration::eulerian4C
//         hkl::angleConfiguration::eulerian6C
//         hkl::angleConfiguration::kappa4C
//         hkl::diffractometer
//         hkl::diffractometer::eulerian4C
//         hkl::diffractometer::eulerian6C
//         hkl::diffractometer::kappa4C
//         hkl::mode
//         hkl::mode::eulerian4C
//         hkl::mode::eulerian6C
//         hkl::mode::eulerian6C::horizontal4C
//         hkl::mode::eulerian6C::vertical4C
//
//   AFFINEMENT
//     + Simplex method
//     + optimisation du Simplex en ajoutant le champ m_hkl_phi à la class Reflection.
//
//   ANGLECONFIGURATION
//     + création des classes Eulerian4C Eulerian6C Kappa4C
//
//   AXE
//     + derive Axe de Quaternion afin d'accélérer les calcules de getQ dans les différentes classes.
//
//   DIFFRACTOMETRE
//     + class Eulerian4C
//     + class Eulerian6C
//
//   MODES
//     + Ajout d'un champ commentaire pour décrire le mode et sa configuration.
//     + Mettre les paramètres de configuration sous forme de #Value pour pouvoir les nommer.
//     + Modifier la fonction computeAngles pour utiliser une référence sur aC et non un pointeur.
//     + Scinder le fichier mode.h en plusieurs suivant les diffractomètres.
//     - E4C
//       + Mode "Bissector"
//       + Mode "Delta Omega"
//       + Mode "Constant Omega"
//       + Mode "Constant Chi"
//       + Mode "Constant Phi"
//     - E6C
//       + Mode "Horizontal Eulerian 4C Bissector"
//       + Mode "Horizontal Eulerian 4C Delta Omega"
//       + Mode "horizontal Eulerian 4C Constant Omega"
//       + Mode "Horizontal Eulerian 4C Constant Chi"
//       + Mode "Horizontal Eulerian 4C Constant Phi"
//       + Mode "Vertical Eulerian 4C Bissector"
//       + Mode "Vertical Eulerian 4C Delta Omega"
//       + Mode "Vertical Eulerian 4C Constant Omega"
//       + Mode "Vertical Eulerian 4C Constant Chi"
//       + Mode "Vertical Eulerian 4C Constant Phi"
//
//   REFLECTIONS
//     + Ajout d'un champ m_hkl_phi ( R-1 * Q ) qui permet d'accélérer énormément le simplex.
//
//   DOCUMENTATION
//     + Réorganiser la mainpage de la documentation en plusieurs pages.
//     ~ API
//
//   BINDING
//     ~ python
//
//   FRONTEND
//     ~ Developper une interface graphique à la librairie pour la tester.
//
// Revision 1.1.2.32  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.1.2.31  2005/08/01 16:17:57  picca
// en cours de modification des classes myvector et mymap
//
// Revision 1.1.2.30  2005/07/25 15:54:31  picca
// L'affinement fonctionne.
//
// Revision 1.1.2.29  2005/07/22 14:57:16  picca
// en cours d'optimisation du fit
//
// Revision 1.1.2.28  2005/07/22 07:53:50  picca
// Now affinement_simplex is working
//
// Revision 1.1.2.27  2005/07/18 16:16:11  picca
// -ajout test fonctions pour Crystal:
// 	operator +=, -=, *=, /=, -, *
//
// Revision 1.1.2.26  2005/07/13 16:00:03  picca
// Travail en cours sur l'affinement
//
// Revision 1.1.2.25  2005/07/12 16:07:20  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.24  2005/06/23 16:29:04  picca
// Juste avant le depart pour montreal.
//
// Revision 1.1.2.23  2005/06/23 10:00:44  picca
// Modification de la classe angleconfiguration pour permettre d'utiliser le même
// axe à la fois dans la partie sample et la partie detector.
//
// Revision 1.1.2.22  2005/06/21 13:12:40  picca
// ajout du template MyMap
//
// Revision 1.1.2.21  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.1.2.20  2005/06/20 07:08:10  picca
// affinement suite
//
// Revision 1.1.2.19  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.1.2.18  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.1.2.17  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.16  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.1.2.15  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.14  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.1.2.13  2005/04/12 07:09:51  picca
// Réécriture de la classe diffractoemeter
//
// Revision 1.1.2.12  2005/04/07 09:13:36  picca
// rewrite of diffractoemter_test
//
// Revision 1.1.2.11  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.1.2.10  2005/04/06 14:13:57  picca
// *** empty log message ***
//
// Revision 1.1.2.8  2005/04/04 14:22:38  picca
// modification de la classe Crystal.
// -ajout des tests unitaires.
//
// Revision 1.1.2.7  2005/04/01 14:59:01  picca
// Ajout de la classe Lattice qui contient les parametres cristallins des crystaux
//
// Revision 1.1.2.6  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.5  2005/03/31 14:30:42  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.4  2005/03/11 13:47:58  picca
// Ajout du constructeur par default cristal()
//
// Revision 1.1.2.3  2005/03/03 07:35:46  picca
// faute de frappe :)
//
// Revision 1.1.2.2  2005/03/02 08:11:15  picca
// ajout de l'opérateur << et == pour la classe cristal.
// Suppression de la méthode printOnScreen
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
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

#include "config.h"

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include "mymap.h"
#include "svecmat.h"
#include "reflection.h"
#include "fitparameter.h"
#include "fitparameterlist.h"
#include "HKLException.h"

namespace hkl {

/**
 * @brief Class which store the cristals parameters
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
class Crystal : public FitParameterList, public Object
{

public:

  /**
   * @brief default constructor
   */
  Crystal();
  
  /**
   * @brief default constructor
   * @param name The name of the crystal
   */
  Crystal(std::string const & name);
  
  /**
   * @brief Copy constructor
   * @param C The crystal we want to copy.
   *
   * This constructor creates a new crystal from the C crystal.
   */
  Crystal(Crystal const & C);

  smatrix const & get_B(void) const {return m_B;} //!< get the m_B %smatrix
  smatrix const & get_U(void) const {return m_U;} //!< get the m_U %smatrix
  ReflectionList const & get_reflectionList(void) const {return m_reflectionList;} //!< get the reflectionList
  
  void set_B(smatrix const & m) {m_B = m;} //!< set the B matrix;
  void set_U(smatrix const & m) {m_U = m;} //!< set the U matrix;
  void set_reflectionList(ReflectionList const & reflectionList) {m_reflectionList = reflectionList;} //!< set the reflectionList
 
  /**
   * @brief Return All the Crystal Lattice parameters
   * @param[out] a
   * @param[out] b
   * @param[out] c
   * @param[out] alpha
   * @param[out] beta
   * @param[out] gamma
   */
  void getLattice(double * a, double * b, double * c,
                  double * alpha, double * beta, double * gamma) const;
  
  void setLattice(double const & a, double const & b, double const & c,
                  double const & alpha, double const & beta, double const & gamma); //!< set the crystal parameters
  
  /**
   * @brief Compute the reciprocal lattice of the Crystal
   * @param[out] a_star
   * @param[out] b_star
   * @param[out] c_star
   * @param[out] alpha_star
   * @param[out] beta_star
   * @param[out] gamma_star
   */
  void getReciprocalLattice(double * a_star, double * b_star, double * c_star,
                            double * alpha_star, double * beta_star, double * gamma_star) const;

  /**
   * @brief Add a reflection to the reflection liste
   * @param reflection the %Reflection to add.
   * @return the index of the added %Reflection.
   */
  unsigned int addReflection(Reflection const & reflection);
  
  /**
   * @brief Delete the index reflection from the reflection list
   * @param index the index of the reflection to delete.
   */
  void delReflection(unsigned int const & index) throw (HKLException);
  
  /**
   * @brief Modification of the ith reflections
   * @param index of the reflection to modify
   * @param reflection replace the reflection with that one.
   */
  void setReflection(unsigned int const & index, Reflection const & reflection) throw (HKLException);
  
  /**
   * @brief Get a constant reference on a reflection.
   * @param index of the reflection.
   * @return A constant reference on the #m_reflectionList.
   */
  Reflection & getReflection(unsigned int const & index) throw (HKLException);
  
  /**
   * @brief Get a constant reference on a reflection.
   * @param index of the reflection.
   * @return A constant reference on the #m_reflectionList.
   */
  Reflection const & getReflection(unsigned int const & index) const throw (HKLException);
  
  /**
   * @brief Return true or false if the crystal contain at least nb_reflections independant reflections.
   * @param nb_reflections the minimim number of independant reflections.
   * @return true if crystal contain at least nb_reflections independant reflections. false otherwise.
   *
   * We comptabilize Colinear reflections as one unique reflection available for computation.
   * (ex (1,0,0) et (2,0,0)).
   */
  bool isEnoughReflections(unsigned int nb_reflections) const;

  /**
   * @brief Compute the orientation matrix from two basic non-parallel reflections.
   *
   * Compute the orientation matrix from two basic non-parallel reflections.
   */
  void computeU(void) throw (HKLException);

  /**
   * @brief Compute the leastSquare of the crystal.
   * @return the variance.
   */
  double fitness(void) throw (HKLException);
  
  /**
   * @brief Randomize the crystal
   */
  void randomize(void);

  /**
   * @brief overload of the == operator for the cristal class
   * @param C The crystal we want to compare.
   */
  bool operator == (Crystal const & C) const;
 
  /**
   * @brief Print the state of the current crystal on a ostream.
   * @param flux the std::ostream to write into.
   * @return the flux modified.
   */
  std::ostream & printToStream(std::ostream & flux) const;
  
protected:
  smatrix m_B; //!< The crystal matrix.
  smatrix m_U; //!< The Orientation matrix
  ReflectionList m_reflectionList; //!< the reflection list associated with this crystal

  /**
   * @brief The main function to compute the matrix B.
   *
   * This method computes the B matrix from the cristal parameters.
   * It is the transformation matrix from the reciprocal base to an orthonormal
   * one.
   * @f[
   *  B = \left(
   *    \begin{matrix}
   *      b_1 & b_2\cos\beta_3  & b_3\cos\beta_2 \\
   *      0   & b_2\sin\beta_3  & -b_3\sin\beta_2\cos\alpha_1 \\
   *      0   & 0               & 1/a_3
   *    \end{matrix}
   *  \right)
   * @f]
   */
  void _computeB(void);

  void _computeU(void);
  /**
   * @brief Return the index of the next usable reflection for calculation
   * @param from The iterator of the reflection from which the search start.
   * @return The iterator of the next reflection valid for calculus.
   */
  ReflectionList::iterator & _getNextReflectionIteratorForCalculation(ReflectionList::iterator & from) throw (HKLException);
  
};

typedef MyMap<Crystal> CrystalList;

} // namespace hkl

  /**
   * @brief Surcharge de l'operateur << pour la class cristal
   * @param flux 
   * @param C 
   * @return 
   */
std::ostream & operator << (std::ostream & flux, hkl::Crystal const & C);

#endif // _CRISTAL_H_
