//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/angleconfiguration.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class angleConfiguration, angleConfiguration_Eulerian4C, angleConfiguration_Kappa4C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.3 $

//

// $Log: angleconfiguration.h,v $
// Revision 1.3  2005/10/11 14:10:51  picca
// *Modification to take care of the C4786 warning message for the debug version of the library
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.23  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.21  2005/08/29 07:53:37  picca
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
// Revision 1.1.2.20  2005/06/23 10:00:44  picca
// Modification de la classe angleconfiguration pour permettre d'utiliser le même
// axe à la fois dans la partie sample et la partie detector.
//
// Revision 1.1.2.19  2005/06/22 15:04:36  picca
// surcharge de operator[] pour la classe AngleConfiguration
//
// Revision 1.1.2.18  2005/06/21 13:12:40  picca
// ajout du template MyMap
//
// Revision 1.1.2.17  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.16  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.1.2.15  2005/04/22 11:45:44  picca
// Ajout de la fonction setAxeAngle Ã  la classe AngleConfiguration
//
// Revision 1.1.2.14  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.13  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.12  2005/03/31 14:30:42  picca
// Modification de la classe crystal
// - ajout d'un champ m_name S
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.11  2005/03/24 08:36:36  picca
// remplacement des fonction getSampleAxe et getDetectorAxe par getAxe dans la classe angleConfiguration
//
// Revision 1.1.2.10  2005/03/23 16:38:53  picca
// -ajout de computeU dans la classe smatrix.
// -ajout de getQ() dans la classe angleConfiguration
// -Test de getQ().
//
// Revision 1.1.2.9  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.8  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons S
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.7  2005/03/10 14:57:22  picca
// -ajout de la surcharge de l'operateur== pour le type abstrait angleConfiguration.
// - ajout des test sur cet opérateur
//
// Revision 1.1.2.6  2005/03/03 18:43:21  picca
// remplacement des noms de classe
// angleconfiguration_eulerian4C -> angleConfiguration_Eulerian4C
//
// idem pour kappa et 6C
//
// Revision 1.1.2.5  2005/03/02 18:31:45  picca
// ajout des test unitaires pour la classe angleconfiguration
//
// Revision 1.1.2.1  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.10  2005/02/10 16:47:04  picca
// documentation update
//
// Revision 1.9  2005/01/27 09:23:53  delos
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
/// Store the current angle configuration according to the type of diffractometer.

#ifndef _ANGLECONFIGURATION_H_
#define _ANGLECONFIGURATION_H_

#include "config.h" // uniquement pour le pragma

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "svecmat.h"
#include "quaternion.h"
#include "constants.h"
#include "axe.h"
#include "HKLException.h"

namespace hkl {

/**
 * \brief Store the current angle configuration according to the type of diffractometer.
 * 
 * This class will be derived depending of the geometry of the diffractometers.
 * this class is an "abstraite" class.
 * 
 * \todo Remplacer axeMap par un unsorted_map (GCC 4.0) et rajouter un champ type d'axe pour accelerer les calcules.
 */
class AngleConfiguration
{
public:
  
  /**
   * \brief The default constructor
   */
  AngleConfiguration();
  
  /**
   * \brief The copy constructor
   */
  AngleConfiguration(AngleConfiguration const & aC);
  
  
  /**
   * \brief The destructor
   */
  virtual ~AngleConfiguration();
 
  /**
   * \brief Compare two angleConfiguration
   */
  bool operator==(AngleConfiguration const & aC) const;
 

  /**
   * \brief put the angleConfiguration into a stream
   * \param flux
   */
  std::ostream & printToStream(std::ostream & flux) const;
  
  /**
   * \brief get a reference on the %hkl::Axe
   * \param name the name of the %hkl::Axe we are looking for
   *
   * Return a reference on the axe with the right name.
   * If the axe can not be find, it raise an HKLException
   */
  Axe & operator[](std::string const & name) throw (HKLException);

  /**
   * \brief get  the axe angle
   * \param name the name of the axe we are looking for
   *
   * Return a reference on the axe with the right name.
   * If the axe can not be find, it raise an HKLException
   */
  Axe const & operator[](std::string const & name) const throw (HKLException);

  /**
   * \brief Return a vector of string with the name of all axes
   */
  std::vector<std::string> const getAxesNames() const;
  
  /**
   * \brief  Add a new Axe into the m_samples vector
   * \param A the Axe
   */
  void addSampleAxe(Axe const & A) throw (HKLException);
  
  /**
   * \brief  Add a new Axe into the m_detectors vector
   * \param A the Axe
   */
  void addDetectorAxe(Axe const & A) throw (HKLException);

  /**
   * \brief return the Rotatio matrix of the angleConfiguration
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples vector.
   * So we can describe every diffractometer if we put the Axe in the right position into this vector
   */
  smatrix getSampleRotationMatrix() const;
 
  /**
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q vector
   */
  svector getQ(Quaternion const & qi) const;

  /**
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the HKLphi vector
   */
  svector getHKLphi(Quaternion const & qi) const;

private:
  AxeMap m_axeMap; //< The map containing all the axes.
  std::vector<std::string> m_samples; //< The sample vector
  std::vector<std::string> m_detectors; //< the detector vector
};

} // namespace hkl

/**
  * \brief Surcharge de l'operateur << pour la class angleconfiguration
  * @param flux 
  * @param aC
  *
  * This function use the printToStream virtual function to print on screen
  * or in an ostream. Because the operator<< can not be declare as virtual
  * we need to use this hake to virtualize not the operator<< but the function
  * called by it printToStream
  */
std::ostream & operator<<(std::ostream & flux, hkl::AngleConfiguration const & aC);

#endif
