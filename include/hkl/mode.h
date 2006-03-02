//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/hkl/mode.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class mode, eulerian_bissectorMode4C, eulerian_constantOmegaMode4C,
// eulerian_horizontal4CBissectorMode6C, eulerian_vertical4CBissectorMode6C, eulerian_lifting3CDetectorMode6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.2 $

//

// $Log: mode.h,v $
// Revision 1.2  2006/01/26 14:24:51  picca
// * update documentation
//
// Revision 1.1  2006/01/24 16:18:30  picca
// *move the includes files
//
// Revision 1.8  2006/01/23 16:14:55  picca
// * now diffractometer serialization works!!!
//
// Revision 1.7  2006/01/06 16:24:29  picca
// * modification of the bksys files
//
// Revision 1.6  2005/11/25 14:01:46  picca
// * add getCrystalParametersNames
//
// Revision 1.5  2005/11/14 13:34:14  picca
// * update the Simplex method.
//
// Revision 1.4  2005/10/26 15:54:50  picca
// * derive Mode from ObjectwithParameters
// * update uml diagram
//
// Revision 1.3  2005/10/26 15:11:41  picca
// * AngleConfiguration -> Geometry
// * add PseudoAxe class
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.25  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.22  2005/08/29 07:53:37  picca
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
// Revision 1.1.2.21  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.1.2.20  2005/07/27 15:50:35  picca
// ajout des test de la partie mode du diffractometre.
//
// Revision 1.1.2.19  2005/07/26 16:09:04  picca
// en cours de travail sur la partie mode du diffractometre.
//
// Revision 1.1.2.18  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.17  2005/06/22 15:04:36  picca
// surcharge de operator[] pour la classe AngleConfiguration
//
// Revision 1.1.2.16  2005/06/21 16:17:26  picca
// modification de MyMap pour prendre en compte le bug de specialisation des templates de VC++6
//
// Revision 1.1.2.15  2005/06/21 15:35:18  picca
// Surcharge du template template MyMap pour prendre en compte les map de pointeur
//
// Revision 1.1.2.14  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.1.2.13  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.12  2005/04/22 11:45:44  picca
// Ajout de la fonction setAxeAngle Ã  la classe AngleConfiguration
//
// Revision 1.1.2.11  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.10  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.1.2.9  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.8  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.7  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.6  2005/03/11 09:41:02  picca
// -changement de nom des classes eulerianDiffractometer4C -> diffractometer_Eulerian4C
// idem pour eulerian 6C et kappa 4C
//
// Revision 1.1.2.5  2005/03/03 18:43:21  picca
// remplacement des noms de classe
// angleconfiguration_eulerian4C -> angleConfiguration_Eulerian4C
//
// idem pour kappa et 6C
//
// Revision 1.1.2.4  2005/03/02 12:43:57  picca
// add the operator<< for the anglecalculation classes.
//
// Revision 1.1.2.3  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.1.2.2  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.17  2005/02/11 15:52:45  picca
// documentation
//
// Revision 1.16  2005/01/27 09:23:53  delos
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

/// Define the way we use the diffractometer.


#ifndef _MODE_H_
#define _MODE_H_

#include "config.h"

#include <iostream>

#include "mymap.h"
#include "value.h"
#include "svecmat.h"
#include "myvector.h"
#include "geometry.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {
  /*!
   * \brief This class defines how to use a diffractomer.
   */
  class Mode : public ObjectWithParameters
  {
    public:

      virtual ~Mode(void); //!< The default constructor

      /*!
       * \brief The main function to get a sample of angles from (h,k,l).
       * \param h The scaterring vector first element.
       * \param k The scaterring vector second element.
       * \param l The scaterring vector third element.
       * \param UB The product of the orientation matrix U by the crystal matrix B.
       * \param[out] geometry The Geometry to compute.
       */
      virtual void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const = 0;
      
      /*!
       * \brief Print the state of the current Mode on a ostream.
       * \param flux
       * \return the flux modified.
       */
      ostream & printToStream(ostream & flux) const;
      
    public:

      Mode(void); //!< Default constructor - protected to make sure this class is abstract.
  };

#ifdef MSVC6
  typedef MyStarMap<Mode*> ModeList;
#else
  typedef MyMap<Mode*> ModeList; //!< \typedef a MyMap containing pointers of Mode.
#endif

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << for the Mode class
 * \param flux The flux to modifie 
 * \param mode The mode to stream.
 * \return The modified flux.
 */
ostream & operator << (ostream & flux, hkl::Mode const & mode);

#endif // _MODE_H_
