//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/reflection.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class reflection

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.2 $

//

// $Log: reflection.h,v $
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.20  2005/08/29 07:53:37  picca
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
// Revision 1.1.2.19  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.18  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.1.2.17  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.1.2.16  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.1.2.15  2005/05/27 12:30:34  picca
// class: Reflection
// 	- ajout contructeur par default
// 	- ajout set(const Reflection & reflection) pour mettre Ã  jour un reflection Ã  partir d'une autre
// 	- ajout get_angleConfiguration et set_valueConfiguration
// 	- ajout get_source, set_source
// 	- remplacement getRelevance par get_relevance
// 	- remplacement getFlag par get_flag
//
// Revision 1.1.2.14  2005/05/26 14:36:54  picca
// class diffractometer
// - ajout getLattice, setLattice, getReciprocalLattice
// -ajout getCrystalLattice, setCrystalLattice, getCrystalReciprocalLattice
// -ajout des test correspondants.
// -ajout getReflection, getCrystalReflection
// -ajout d'un binding pour python de la classe diffractometer qui utilise la librairie boost_python
// -ajout d'un GUI en python + PyGtk
//
// Revision 1.1.2.13  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.12  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.1.2.11  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.1.2.10  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.9  2005/03/31 14:30:42  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.8  2005/03/31 11:28:26  picca
// Modification de la classe Reflection.
// - ajout de 2 champs:
// 	m_source pour sauvegarder l'Ã©tat de la source pour chaque reflection.
// 	m_flag pour indiquer si oui ou non on utilise la reflection dans le calcule de U
// - ajout des getSet pour tous les champs de reflection.
// - ajout des test de ces getSet.
//
// Revision 1.1.2.7  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.6  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.5  2005/03/10 15:20:01  picca
// -Ajout dans operateur == du test sur m_setOfAngles
//
// Revision 1.1.2.4  2005/03/10 10:00:15  picca
// -add a static string m_strRelevance to display the relevance in a human readable way.
//
// modified the operator<<  to display the string and no mode int reflection::relevance
//
// Revision 1.1.2.3  2005/03/03 09:17:33  picca
// Ajout de la surcharge de l'operateur<< pour la classe reflection.
// Ajout de la surcharge de l'opérateur== pour la classe reflection. On ne prend pas encore en compte la comparaison de la classe angleconfiguration qui est à revoir.
// Suppression de la la fonction printOnScreen remplacée par la première surcharge.
//
// Revision 1.1.2.2  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.8  2005/02/08 17:03:08  picca
// update the documentation
//
// Revision 1.7  2005/02/08 15:51:05  picca
// update the documenattion
//
// Revision 1.6  2005/01/27 09:23:53  delos
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
#ifndef _REFLECTION_H_
#define _REFLECTION_H_

#include <math.h>
#include <iostream>
#include <string>
#include "angleconfiguration.h"
#include "source.h"
#include "svecmat.h"

namespace hkl {

/**
 * \brief The class reflection defines a configuration where a diffraction occurs. It
 * 
 * is defined by a set of angles, the 3 integers associated to the reciprocal
 * lattice and its relevance to make sure we only take into account significant
 * reflections.
 */
class Reflection
{
public:
  /**
   * \brief The enumeration "relevance" to make sure we only take into account significant reflections.
   */
  enum Relevance
  {
    notVerySignificant = 0, //!< not very significant reflection
    Significant, //!< significant reflection
    VerySignificant, //!< very significant reflection
    Best //!< Best reflection
  };
  
  /**
   * \brief default constructor
   */
  Reflection(void);
  
  /**
   * \brief copy constructor
   */
  Reflection(Reflection const & reflection);
  
  /**
   * \brief Constructor from parameters
   * \param aC The %AngleConfiguration of the reflection.
   * \param s the %Source states for this reflection.
   * \param h the h number of the reflection.
   * \param k the k number of the reflection.
   * \param l the l number of the reflection.
   * \param relevance %Relevance the relevance of this reflection.
   * \param flag true if the reflection is use during calculation.
   *
   * Create a new reflection and populate is members with the parameters
   */
  Reflection(AngleConfiguration const & aC,
             Source const & s,
             double const & h,
             double const & k,
             double const & l,
             int const & relevance,
             bool const & flag);

  /**
   * \brief Destructor
   *
   * Release the memory of the reflection this
   */
  virtual ~Reflection();

  /**
   * \brief overload of the == operator for the reflection class
   * @param r The reflection we want to compare.
   */
  bool operator == (Reflection const & r) const;
   
  AngleConfiguration & get_angleConfiguration(void) {return m_aC;} //!< get the angle configuration
  AngleConfiguration const & get_angleConfiguration(void) const {return m_aC;} //!< get the angle configuration
  Source const & get_source(void) const {return m_source;} //!< get the source parameters of the reflection
  double const & get_h(void) const {return m_h;} //!< get the h parameter of the reflection
  double const & get_k(void) const {return m_k;} //!< get the k parameter of the reflection
  double const & get_l(void) const {return m_l;} //!< get the l parameter of the reflection
  int const & get_relevance(void) const {return m_relevance;} //!< get the relevance parameter of the reflection
  bool const & get_flag(void) const {return m_flag;} //!< is the reflection use during the U calculation
  svector const & get_hkl_phi(void) const {return m_hkl_phi;} //!< Get the hkl_phi of the reflection
  
  void set_angleConfiguration(AngleConfiguration const & aC); //!< set angleConfiguration
  void set_source(Source const & source); //!< set source
  void set_h(double const & h) {m_h = h;} //!< set h
  void set_k(double const & k) {m_k = k;} //!< set k
  void set_l(double const & l) {m_l = l;} //!< set l
  void set_relevance(int const & relevance) {m_relevance = relevance;} //!< set relevance
  void set_flag(bool const & flag) {m_flag = flag;} //!< set flag
  
  bool toggle(void); //!< toggle the reflection flag.
  svector getHKL(void) const; //!< return hkl as an %svector.
  
  std::string getStrRelevance(void) const; //!< get the relevance parameter of the reflection as a string
  
  /**
   * \brief compute the angle between two reflections
   * \param h2 the h parameters of the second reflection
   * \param k2 the k parameters of the second reflection
   * \param l2 the l parameters of the second reflection
   *
   * Compute the angle between two reflections to get an idea about their level
   * of relevance (return the absolute value). As an example it can detect if
   * (m_h, m_k, m_l) and (h2, k2, l2) are parallel.
   */
  double computeAngle(double const & h2, double const & k2, double const & l2) const;
   
  /**
   * \brief return the Rotatio matrix of the angleConfiguration
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples vector.
   * So we can describe every diffractometer if we put the Axe in the right position into this vector
   */
  smatrix getSampleRotationMatrix(void) const;
 
  /**
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q vector
   */
  svector getQ(void) const;

protected:
  AngleConfiguration m_aC; //!< The corresponding angle configuration.
  Source m_source; //!< The corresponding source parameters.
  double m_h; //!< The first of the three numbers (h,k,l).
  double m_k; //!< The second of the three numbers (h,k,l).
  double m_l; //!< The third of the three numbers (h,k,l).
  int m_relevance; //!< Its associated relevance. 
  bool m_flag; //!< is the reflection use for calculation.
  static std::string m_strRelevance[]; //<! the string vector which contain the relevance in human readable way.

private:  
  svector m_hkl_phi; //!< juste utiliser pour accélérer les calcules de fitness des cristaux.
};

typedef std::vector<Reflection> ReflectionList;

} // namespace hkl

/**
  * \brief Surcharge de l'operateur << pour la class reflection
  * @param flux The flux to print into
  * @param r
  */
std::ostream & operator << (std::ostream & flux, hkl::Reflection const & r);

#endif // _REFLECTION_H_
