//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/mode_eulerian4C.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class eulerian_bissectormode4C, eulerian_constantOmega4C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.3 $

//

// $Log: mode_eulerian4C.cpp,v $
// Revision 1.3  2005/10/26 15:11:41  picca
// * AngleConfiguration -> Geometry
// * add PseudoAxe class
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.20  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.18  2005/08/29 07:53:37  picca
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
//     + Modifier la fonction computeAngles pour utiliser une référence sur geometry et non un pointeur.
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
// Revision 1.1.2.17  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.1.2.16  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.1.2.15  2005/07/27 15:50:35  picca
// ajout des test de la partie mode du diffractometre.
//
// Revision 1.1.2.14  2005/07/26 16:09:04  picca
// en cours de travail sur la partie mode du diffractometre.
//
// Revision 1.1.2.13  2005/06/22 15:04:36  picca
// surcharge de operator[] pour la classe Geometry
//
// Revision 1.1.2.12  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.1.2.11  2005/06/09 13:41:10  picca
// ajout de la class object et reecriture de la class Axe derivant de cette class object
//
// Revision 1.1.2.10  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.9  2005/04/22 11:45:45  picca
// Ajout de la fonction setAxeAngle Ã  la classe Geometry
//
// Revision 1.1.2.8  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.1.2.7  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.6  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.5  2005/03/24 08:36:36  picca
// remplacement des fonction getSampleAxe et getDetectorAxe par getAxe dans la classe angleConfiguration
//
// Revision 1.1.2.4  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply_Eulerian4C
//
// Revision 1.1.2.3  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.2  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.1  2005/03/09 12:48:36  picca
// Changement de nom des fichier contenant les definitions des modes.
//
// Revision 1.15.2.3  2005/03/03 18:43:21  picca
// remplacement des noms de classe
// angleconfiguration_eulerian4C -> angleConfiguration_Eulerian4C
//
// idem pour kappa et 6C
//
// Revision 1.15.2.2  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.15.2.1  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.15  2005/01/27 09:23:53  delos
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
#include "mode_eulerian4C.h"

namespace hkl {
  namespace mode {

    Eulerian4C::Eulerian4C(void)
    {}

    Eulerian4C::~Eulerian4C(void)
    {}


    namespace eulerian4C {
      /******************/
      /* BISSECTOR MODE */
      /******************/
      Bissector::Bissector(void)
      {
        set_name("Bissector");
        set_description("Omega = 2theta / 2. \n there is no parameters for this mode.");
      }

      Bissector::~Bissector(void) {}

      void
        Bissector::computeAngles(double h, double k, double l,
            const smatrix & UB,
            double lambda,
            Geometry & geometry) const throw (HKLException)
        {
          // Calcule de Theta
          double theta;
          svector hphi = UB * svector(h,k,l);
          try {
            theta = _asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } catch (HKLException const &) {
            throw HKLException("Unobtainable reflection",
                "Please change h k l values or the energy.",
                "Mode_Eulerian4C_bissector::computeAngles");
          }

          // Calcule de Omega
          double omega = theta;

          // Calcule de Chi
          double s_chi = hphi[1];
          double c_chi = hphi[0]*hphi[0]+hphi[2]*hphi[2];
          if (c_chi < 0.)
            throw HKLException("Unobtainable reflection.",
                "Change h k l values",
                "Mode_Eulerian4C_Bissector::computeAngles");
          else
            c_chi = sqrt(c_chi);
          double chi = _atan2(s_chi, c_chi);

          // Calcule de Phi
          double s_phi = hphi[0];
          double c_phi = hphi[2];
          double phi = _atan2(s_phi, c_phi);

          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);
          geometry.get_axe("2theta").set_value(2.*theta);
        }

      /***************/
      /* DELTA THETA */
      /***************/

      Delta_Theta::Delta_Theta(void)
      {
        set_name("Delta Theta");
        set_description("Omega = theta + dtheta \n dtheta is the only one parameter of this mode.");
        _addParameter("delta theta");
      }

      Delta_Theta::~Delta_Theta(void)
      {}

      void
        Delta_Theta::computeAngles(double h, double k, double l,
            const smatrix & UB,
            double lambda,
            Geometry & geometry) const throw (HKLException)
        {
          // Calcule de Theta
          double theta;
          svector hphi = UB * svector(h,k,l);
          try {
            theta = _asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } catch (const HKLException &) {
            throw HKLException("Unobtainable reflection",
                "Please change h k l values or the energy.",
                "Mode_Eulerian4C_Delta_Theta::computeAngles");
          }

          // Calcule de Omega
          // By definition in 4C omega constant mode.
          double dtheta = getParameterValue("delta theta");
          double omega = theta + dtheta;

          // Calcule de Chi
          double s_chi = hphi[1];
          double c_chi = hphi[0]*hphi[0]-hphi[1]*hphi[1]*tan(dtheta)*tan(dtheta)+hphi[2]*hphi[2];
          if (c_chi < 0.)
            throw HKLException("Unobtainable reflection.",
                "Change h k l values",
                "Mode_Eulerian4C_Delta_Theta::computeAngles");
          else
            c_chi = sqrt(c_chi) * cos(dtheta);
          double chi = _atan2(s_chi, c_chi);

          // Calcule de Phi
          double s_phi = hphi[0]*cos(dtheta)*cos(chi)-hphi[2]*sin(dtheta);
          double c_phi = hphi[2]*cos(dtheta)*cos(chi)+hphi[0]*sin(dtheta);
          double phi = _atan2(s_phi, c_phi);

          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);
          geometry.get_axe("2theta").set_value(2.*theta);
        }

      /******************/
      /* CONSTANT OMEGA */
      /******************/

      Constant_Omega::Constant_Omega(void)
      {
        set_name("Constant Omega");
        set_description("Omega = Constante \n  omega is the only one parameter of this mode.");
        _addParameter("omega");
      }

      Constant_Omega::~Constant_Omega(void)
      {}

      void
        Constant_Omega::computeAngles(double h, double k, double l,
            const smatrix & UB,
            double lambda,
            Geometry & geometry) const throw (HKLException)
        {
          // calcule de Theta
          double theta;
          svector hphi = UB * svector(h,k,l);
          try {
            theta = _asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } catch (const HKLException &) {
            throw HKLException("sine bigger than 1.",
                "Maybe error in UB matrix",
                "Mode_Eulerian4C_Constant_Omega::computeAngles");
          }

          // La définition de omega dans ce mode.
          double omega = getParameterValue("omega");

          // calcule de Chi.
          double s_chi = hphi[1];
          double c_chi = (hphi[0]*hphi[0] + hphi[2]*hphi[2])*cos(omega-theta)*cos(omega-theta)-hphi[1]*hphi[1]*sin(omega-theta)*sin(omega-theta);
          if (c_chi < 0.)
            throw HKLException("Unobtainable reflection.",
                "Change h k l values",
                "Mode_Eulerian4C_Constant_Omega::computeAngles");
          else
            c_chi = sqrt(c_chi);
          double chi = _atan2(s_chi, c_chi);

          // Calcule de Phi
          double s_phi = hphi[0]*cos(chi)*cos(omega - theta) - hphi[2]*sin(omega - theta);
          double c_phi = hphi[0]*sin(omega - theta) + hphi[2]*cos(chi)*cos(omega - theta);
          double phi = _atan2(s_phi, c_phi);

          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);
          geometry.get_axe("2theta").set_value(2.*theta);
        }

      /****************/
      /* CONSTANT CHI */
      /****************/

      Constant_Chi::Constant_Chi(void)
      {
        set_name("Constant Chi");
        set_description("chi = Constante \n  chi is the only one parameter of this mode.");
        _addParameter("chi");
      }

      Constant_Chi::~Constant_Chi(void)
      {}

      void
        Constant_Chi::computeAngles(double h, double k, double l,
            const smatrix & UB,
            double lambda,
            Geometry & geometry) const throw (HKLException)
        {
          // calcule de hphi
          svector hphi = UB * svector(h,k,l);

          // calcule de Theta
          double theta;
          try {
            theta = _asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } catch (const HKLException &) {
            throw HKLException("sine bigger than 1.",
                "Maybe error in UB matrix",
                "Mode_Eulerian4C_Constant_Chi::computeAngles");
          }

          // La définition de chi dans ce mode.
          double chi = getParameterValue("chi");
          //! \todo traiter le cas C=0;

          // calcule de Omega.
          double s_omega_theta = (hphi[0]*hphi[0] + hphi[2]*hphi[2])*sin(chi)*sin(chi) - hphi[1]*hphi[1]*cos(chi)*cos(chi);
          double c_omega_theta = hphi[1];
          if (s_omega_theta < 0.)
            throw HKLException("Unobtainable reflection.",
                "Change h k l values",
                "Mode_Eulerian4C_Constant_Chi::computeAngles");
          else
            s_omega_theta = sqrt(s_omega_theta);
          double omega = _atan2(s_omega_theta, c_omega_theta) + theta;

          // Calcule de Phi
          double s_phi = hphi[0]*cos(chi)*cos(omega - theta) - hphi[2]*sin(omega - theta);
          double c_phi = hphi[0]*sin(omega - theta) + hphi[2]*cos(chi)*cos(omega - theta);
          double phi = _atan2(s_phi, c_phi);

          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);
          geometry.get_axe("2theta").set_value(2.*theta);
        }

      /****************/
      /* CONSTANT PHI */
      /****************/

      Constant_Phi::Constant_Phi(void)
      {
        set_name("Constant Phi");
        set_description("phi = Constante \n  phi is the only one parameter of this mode.");
        _addParameter("phi");
      }

      Constant_Phi::~Constant_Phi(void)
      {}

      void
        Constant_Phi::computeAngles(double h, double k, double l,
            const smatrix & UB,
            double lambda,
            Geometry & geometry) const throw (HKLException)
        {
          // calcule de Theta
          double theta;
          svector hphi = UB * svector(h,k,l);
          try {
            theta = _asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } catch (const HKLException &) {
            throw HKLException("sine bigger than 1.",
                "Maybe error in UB matrix",
                "Mode_Eulerian4C_Constant_Chi::computeAngles");
          }

          // La définition de chi dans ce mode.
          double phi = getParameterValue("phi");

          // calcule de Omega.
          double s_omega_theta = hphi[0]*cos(phi)-hphi[2]*sin(phi);
          double c_omega_theta = hphi[0]*hphi[0]*sin(phi)*sin(phi)+hphi[1]*hphi[1]+hphi[2]*hphi[2]*cos(phi)*cos(phi)+hphi[0]*hphi[2]*cos(phi)*sin(phi);
          if (c_omega_theta < 0.)
            throw HKLException("Unobtainable reflection.",
                "Change h k l values",
                "Mode_Eulerian4C_Constant_Chi::computeAngles");
          else
            c_omega_theta = sqrt(c_omega_theta);
          double omega = _atan2(s_omega_theta, c_omega_theta) + theta;

          // Calcule de Chi
          double s_chi = hphi[1];
          double c_chi = hphi[0]*sin(phi) + hphi[2]*cos(phi);
          double chi = _atan2(s_chi, c_chi);

          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);
          geometry.get_axe("2theta").set_value(2.*theta);
        }
    } // namespace eulerian4C
  } // namespace mode
} // namespace hkl
