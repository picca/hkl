//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/source.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class source

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.2 $

//

// $Log: source.h,v $
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.8  2005/08/29 07:53:37  picca
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
// Revision 1.1.2.7  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.6  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.5  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.4  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.3  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.1.2.2  2005/03/02 08:12:21  picca
// Ajout de:
// operator<< et operator==
// suppression de printOnScreen
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.10  2005/02/08 15:51:05  picca
// update the documenattion
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

#ifndef SOURCE_H
#define SOURCE_H

#include <iostream>
#include "svecmat.h"
#include "quaternion.h"

namespace hkl {

/**
 * @brief describe the source parameters
 * 
 * The class source defines a light ray and its main characteristics.
 */
class Source
{
public:
  /**
   * @brief Default Constructor of the source
   *
   * Create a new source with all is privates parameters set to zero.
   */
  Source(void);

  /**
   * @brief Copy constructor
   * @param source the %Source to copy from.
   *
   * Create a new source by copying the source S.
   * <b>Check if S units are consistent with the crystal units for the diffractometry computations</b>.
   */
  Source(Source const & source);

  /**
   * @brief Constructor from parameters
   * @param waveLength the wavelength of the beam.
   * @param direction the X-rays beam direction. This parameter is normalize by
   * the methode  
   *
   * Create a new source from the parameters.
   * <b>_waveLength unit must be consistent with the crystal length units</b>.
   */
  Source(double const & waveLength, svector const & direction);

  double const & get_waveLength(void) const {return m_waveLength;} //!< Get the waveLength of the source.
  svector const & get_direction(void) const {return m_direction;} //!< Get the direction of the source.
  Quaternion const & get_qi(void) const {return m_qi;} //!< Get the incomming wave quaternion.
  
  /**
   * @brief overload of the == operator for the source class
   * @param source The %Source we want to compare.
   * @return The comparison with the %Source S. 
   */
  bool operator == (Source const & source) const;
 
 /**
   * @brief set the wavelength
   * @param wl the wavelength
   *
   * Set the wavelength of the source
   * <b>wl unit must be consistent with the crystal length units</b>.
   */
  void setWaveLength(double wl);

  /**
   * @brief Set the direction.
   * @param direction
   *
   * The direction is normalize by the methode
   */
  void setDirection(svector const & direction);
  
 /**
   * @brief Get the ki vector
   */
  svector getKi(void) const;
  
 /**
  * \brief set the ki vector
  */
  void setKi(svector const & ki);
  
private:
  /**
   * @brief The wave length of the beam.
   * Has to be defined in a consistent way with the crystal units.
   * 
   * The wave length plays a significant role in diffractometry computations and can be varied.
   * Has to be defined in a consistent way with the crystal units.
   */
  double m_waveLength;
  svector m_direction; //!< The direction of the incomming beam.
  Quaternion m_qi; //!< The incomming wave vector
};

} // namespace hkl

/**
  * @brief Surcharge de l'operateur << pour la class source
  * @param flux 
  * @param S 
  * @return the modified flux.
  */
std::ostream & operator << (std::ostream & flux, hkl::Source const & S);

#endif // SOURCE_H
