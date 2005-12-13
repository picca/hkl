//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/constants.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class constant, physicalConstants, mathematicalConstants

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.3 $

//

// $Log: constants.h,v $
// Revision 1.3  2005/12/13 09:53:53  picca
// * fir windows test compile.
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.7  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.5  2005/08/30 13:49:42  picca
// *** empty log message ***
//
// Revision 1.1.2.4  2005/08/30 13:22:49  picca
// Modification de affinement_simplex  car VC++6.0 n'est pas compliant IEEE 754
//
// Revision 1.1.2.3  2005/08/29 07:53:37  picca
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
// Revision 1.1.2.2  2005/07/25 15:54:31  picca
// L'affinement fonctionne.
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.6  2005/02/10 16:16:12  picca
// mistake in the comment after #endif
//
// Revision 1.5  2005/02/10 14:09:34  picca
// documentation
//
// Revision 1.4  2005/01/27 09:23:53  delos
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
#ifndef _CONSTANTS_H_
#define _CONSTANTS_H_

namespace hkl {
  namespace constant{
    namespace math {
      static double const pi = 3.14159265358979323846; //!< the \f$ \pi \f$ value.
      static double const epsilon_0 = 1.e-6; //!< An \f$ \epsilon \f$ use to compare two angles.
      static double const epsilon_1 = 1.e-10; //!< An \f$ \epsilon \f$ use to compare two doubles.
      static double const tiny = 1e-7; //!< A tiny value.
      static double const degToRad = 0.01745329251994330; //!< A value use to convert angles from degrees to radians.
      static double const radToDeg = 57.2957795130823208; //!< A value use to convert angles from radians to degrees.
      static int const precision = 15; //!< The precision use for the persistance of floats.
      } // namespace math
    namespace physic {
      static double const tau = 1.; //!< The \f$ \tau \f$ constant (1. or \f$ \pi \f$).
    } // namespace physic
  } // namespace constant
} // namespace hkl
      
#endif //_CONSTANTS_H_
