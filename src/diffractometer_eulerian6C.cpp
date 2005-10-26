//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/diffractometer_eulerian6C.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class eulerian_diffractometer6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.3 $

//

// $Log: diffractometer_eulerian6C.cpp,v $
// Revision 1.3  2005/10/26 15:11:41  picca
// * AngleConfiguration -> Geometry
// * add PseudoAxe class
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.13  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.11  2005/08/29 12:35:54  picca
// Modification pour compiler sous VC++6.0
//
// Revision 1.1.2.10  2005/08/29 07:53:37  picca
//   GENERAL
//     + création des namespace:
//         hkl
//         hkl::geometry
//         hkl::geometry::eulerian4C
//         hkl::geometry::eulerian6C
//         hkl::geometry::kappa4C
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
// Revision 1.1.2.9  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.1.2.8  2005/04/12 07:09:52  picca
// Réécriture de la classe diffractoemeter
//
// Revision 1.1.2.7  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.1.2.6  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.5  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.4  2005/03/11 09:41:02  picca
// -changement de nom des classes eulerianDiffractometer4C -> diffractometer_Eulerian4C
// idem pour eulerian 6C et kappa 4C
//
// Revision 1.1.2.3  2005/03/11 08:36:28  picca
// - suppression de printToScreen
// - ajout de operator<< pour la classe diffractometer
// - ajout d-un fonction getReflection qui retourne un pointeur sur la reflection selectionnée. (voir s'il ne faut pas faire une copie pour éviter les fuites mémoires).
//
// Revision 1.1.2.2  2005/03/03 18:43:21  picca
// remplacement des noms de classe
// angleconfiguration_eulerian4C -> geometry_Eulerian4C
//
// idem pour kappa et 6C
//
// Revision 1.1.2.1  2005/03/03 09:48:50  picca
// renommage du fichier eulerian_diffractometer6C.cpp pour la coherence avec les autres fichiers de diffracometres
//
// Revision 1.3.2.2  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.3.2.1  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.3  2005/01/27 09:23:53  delos
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
// Class diffractometer to drive experiments. Reference : 
// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.

#include "diffractometer_eulerian6C.h"

namespace hkl {
  namespace diffractometer {

    // Default constructor.
    Eulerian6C::Eulerian6C() : Diffractometer()
    {
      m_name = "Eulerian 6C Generic Soleil";

      m_geometry = new geometry::Eulerian6C();

      // On met à jour la liste des modes utilisables.
      m_modeList.add( new mode::eulerian6C::horizontal4C::Bissector());
      m_modeList.add( new mode::eulerian6C::horizontal4C::Delta_Theta());
      m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Omega());
      m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Chi());
      m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Phi());
      m_modeList.add( new mode::eulerian6C::vertical4C::Bissector());
      m_modeList.add( new mode::eulerian6C::vertical4C::Delta_Theta());
      m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Omega());
      m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Chi());
      m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Phi());
    }

    // Destructor.
    Eulerian6C::~Eulerian6C()
    {
      delete m_geometry;

      // Ne pas oublier de supprimer les modes.
      ModeList::iterator iter = m_modeList.begin();
      ModeList::iterator last = m_modeList.end();

      while(iter != last){
#ifdef VCPP6
        delete iter->second;
#else
        delete *iter;
#endif
        ++iter;
      }
    }

  } // namespace diffractometer
} // namespace hkl
