//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/mode_eulerian6C.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class eulerian_mode6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.4 $

//

// $Log: mode_eulerian6C.cpp,v $
// Revision 1.4  2005/11/14 13:34:14  picca
// * update the Simplex method.
//
// Revision 1.3  2005/10/26 15:11:41  picca
// * AngleConfiguration -> Geometry
// * add PseudoAxe class
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.17  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.15  2005/08/29 12:35:54  picca
// Modification pour compiler sous VC++6.0
//
// Revision 1.1.2.14  2005/08/29 07:53:37  picca
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
// -Suppression des methodes RightMultiply and LeftMutiply
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
// Revision 1.7.2.3  2005/03/03 18:43:21  picca
// remplacement des noms de classe
// angleconfiguration_eulerian4C -> angleConfiguration_Eulerian4C
//
// idem pour kappa et 6C
//
// Revision 1.7.2.2  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.7.2.1  2005/03/02 09:20:23  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.7  2005/01/27 09:23:53  delos
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

#include "mode_eulerian6C.h"

namespace hkl {
  namespace mode {

    Eulerian6C::Eulerian6C() {}

    Eulerian6C::~Eulerian6C() {}

    namespace eulerian6C {
      namespace horizontal4C {

        /***************************/
        /* HORIZONTAL 4C BISSECTOR */
        /***************************/
        Bissector::Bissector() :
          mode::eulerian4C::Bissector()
        {
          set_name("Horizontal Eulerian 4C Bissector ");
        }

        Bissector::~Bissector() {}

        void 
        Bissector::computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("mu").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("gamma").get_value());
#ifdef VCPP6
          ((mode::eulerian4C::Bissector *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Bissector::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("mu").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("gamma").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /*****************************/
        /* HORIZONTAL 4C DELTA THETA */
        /*****************************/
        Delta_Theta::Delta_Theta() :
          mode::eulerian4C::Delta_Theta()
        {
          set_name("Horizontal Eulerian 4C Delta Theta ");
        }

        Delta_Theta::~Delta_Theta() {}

        void 
        Delta_Theta::computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("mu").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("gamma").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Delta_Theta *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Delta_Theta::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("mu").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("gamma").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /********************************/
        /* HORIZONTAL 4C CONSTANT OMEGA */
        /********************************/
        Constant_Omega::Constant_Omega() :
          mode::eulerian4C::Constant_Omega()
        {
          set_name("Horizontal Eulerian 4C Constant Omega");
        }

        Constant_Omega::~Constant_Omega() {}

        void 
        Constant_Omega::computeAngles(double h, double k, double l,
                                      smatrix const & UB,
                                      Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("mu").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("gamma").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Constant_Omega *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Omega::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("mu").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("gamma").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /******************************/
        /* HORIZONTAL 4C CONSTANT CHI */
        /******************************/
        Constant_Chi::Constant_Chi() :
          mode::eulerian4C::Constant_Chi()
        {
          set_name("Horizontal Eulerian 4C Constant Chi");
        }

        Constant_Chi::~Constant_Chi() {}

        void 
        Constant_Chi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("mu").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("gamma").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Constant_Chi *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Chi::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("mu").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("gamma").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /******************************/
        /* HORIZONTAL 4C CONSTANT PHI */
        /******************************/
        Constant_Phi::Constant_Phi() :
          mode::eulerian4C::Constant_Phi()
        {
          set_name("Horizontal Eulerian 4C Constant Phi");
        }

        Constant_Phi::~Constant_Phi() {}

        void 
        Constant_Phi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("mu").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("gamma").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Constant_Phi *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Phi::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("mu").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("gamma").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }
        
      } // namespace horizontal4C
      namespace vertical4C {

        /*************************/
        /* VERTICAL 4C BISSECTOR */
        /*************************/
        Bissector::Bissector() :
          mode::eulerian4C::Bissector()
        {
          set_name("Vertical Eulerian 4C Bissector");
        }

        Bissector::~Bissector() {}

        void 
        Bissector::computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("omega").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("delta").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Bissector *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Bissector::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("omega").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("delta").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /***************************/
        /* VERTICAL 4C DELTA THETA */
        /***************************/
        Delta_Theta::Delta_Theta() :
          mode::eulerian4C::Delta_Theta()
        {
          set_name("Vertical Eulerian 4C Delta Theta");
        }

        Delta_Theta::~Delta_Theta() {}

        void 
        Delta_Theta::computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("omega").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("delta").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Delta_Theta *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Delta_Theta::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("omega").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("delta").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /******************************/
        /* VERTICAL 4C CONSTANT OMEGA */
        /******************************/
        Constant_Omega::Constant_Omega() :
          mode::eulerian4C::Constant_Omega()
        {
          set_name("Vertical Eulerian 4C Constant Omega");
        }

        Constant_Omega::~Constant_Omega() {}

        void 
        Constant_Omega::computeAngles(double h, double k, double l,
                                      smatrix const & UB,
                                      Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("omega").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("delta").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Constant_Omega *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Omega::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("omega").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("delta").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /****************************/
        /* VERTICAL 4C CONSTANT CHI */
        /****************************/
        Constant_Chi::Constant_Chi() :
          mode::eulerian4C::Constant_Chi()
        {
          set_name("Vertical Eulerian 4C Constant Chi");
        }

        Constant_Chi::~Constant_Chi() {}

        void 
        Constant_Chi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("omega").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("delta").get_value());
#ifdef VCPP6
          ((mode::eulerian4C::Constant_Chi *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Chi::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif
          geometry.get_axe("omega").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("delta").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

        /****************************/
        /* VERTICAL 4C CONSTANT PHI */
        /****************************/
        Constant_Phi::Constant_Phi() :
          mode::eulerian4C::Constant_Phi()
        {
          set_name("Vertical Eulerian 4C Constant Phi");
        }

        Constant_Phi::~Constant_Phi() {}

        void 
        Constant_Phi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
        {
          m_geometry_Eulerian4C.get_axe("omega").set_value(geometry.get_axe("omega").get_value());
          m_geometry_Eulerian4C.get_axe("chi").set_value(geometry.get_axe("chi").get_value());
          m_geometry_Eulerian4C.get_axe("phi").set_value(geometry.get_axe("phi").get_value());
          m_geometry_Eulerian4C.get_axe("2theta").set_value(geometry.get_axe("delta").get_value());

#ifdef VCPP6
          ((mode::eulerian4C::Constant_Phi *)this)->computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#else
          mode::eulerian4C::Constant_Phi::computeAngles(h, k, l, UB, m_geometry_Eulerian4C);
#endif

          geometry.get_axe("omega").set_value(m_geometry_Eulerian4C.get_axe("omega").get_value());
          geometry.get_axe("chi").set_value(m_geometry_Eulerian4C.get_axe("chi").get_value());
          geometry.get_axe("phi").set_value(m_geometry_Eulerian4C.get_axe("phi").get_value());
          geometry.get_axe("delta").set_value(m_geometry_Eulerian4C.get_axe("2theta").get_value());

          return;
        }

      } // namespace vertical4C

      /****************************/
      /* LIFTING 3C DETECTOR MODE */
      /****************************/
      lifting3CDetector::lifting3CDetector() {}

      lifting3CDetector::~lifting3CDetector() {}

      // Solving equation (11) from :
      // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
      // Z11 * hphi1 + Z12 * hphi2 + Z13 * hphi3 = k0*sin(delta)
      // Z21 * hphi1 + Z22 * hphi2 + Z23 * hphi3 = k0*(cos(delta)*cos(nu)-1.)
      // Z31 * hphi1 + Z32 * hphi2 + Z33 * hphi3 = k0*cos(delta)*sin(nu)
      // where k0 = tau/lambda = q/2sin(theta) and
      // eta = chi = phi = 0.
      //
      // delta = arcsin(hphi1 / k0)
      // nu = arccos[(1-Q²/k0²)/(2cos(delta))]
      // sin(mu)*(hphi2²+hphi3²) =-hphi3*k0*(cos(delta)*cos(nu)-1)+hphi2*k0*sin(nu)*cos(delta)
      // cos(mu)*(hphi2²+hphi3²) = hphi2*k0*(cos(delta)*cos(nu)-1)+hphi3*k0*sin(nu)*cos(delta)
      void
      lifting3CDetector::computeAngles(double h, double k, double l,
                                       smatrix const & UB,
                                       Geometry & geometry) const throw (HKLException)
      {
        // h(theta) = R.hphi
        double k0;
        double mu;
        double gamma;
        double omega;
        double chi;
        double phi;
        double delta;
        double theta;
        double cos_delta;
        double sin_theta;
        double cos_2theta;
        svector hphi = UB * svector(h,k,l);
        svector hphi_unitVector = hphi.normalize();
        double hphi_length = hphi.norm2();
        double lambda = geometry.get_source().get_waveLength();

        if ((fabs(hphi[Y]) < constant::math::epsilon_1) &&
            (fabs(hphi[Z]) < constant::math::epsilon_1))
          throw HKLException(
              "Unobtainable reflection",
              "The scattering vector is perpendicular to the light ray",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if (fabs(lambda) < constant::math::epsilon_1)
          throw HKLException(
              "lamdba is null",
              "The wave length has not been set",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if ((fabs(h) < constant::math::epsilon_1) && 
            (fabs(k) < constant::math::epsilon_1) &&
            (fabs(l) < constant::math::epsilon_1))
          throw HKLException(
              "(h,k,l) is null",
              "Check your parameters",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if (hphi.norminf() < constant::math::epsilon_1)
          throw HKLException(
              "hphi is null",
              "The matrix U has been computed from two parallel reflections or the crystal matrix is null",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        k0 = constant::physic::tau / lambda;

        // By definition in lifting 3-circles detector mode.
        omega = 0.;
        chi = 0.;
        phi = 0.;

        /////////////////////
        // Bragg relation //
        ///////////////////
        // sin(theta) = || q || * lambda * 0.5 / tau.
        sin_theta = hphi_length * lambda * 0.5;
        // We have to be consistent with the conventions previously defined when we computed the crystal reciprocal lattice.
        sin_theta = sin_theta / constant::physic::tau;

        if (fabs(sin_theta) > 1.+constant::math::epsilon_1)
          throw HKLException(
              "sine bigger than 1.",
              "hphi_length too big, maybe error in UB matrix",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        theta = asin(sin_theta);

        cos_2theta = cos(2*theta);

        // STEP NUMBER 1 : COMPUTING MU
        // We know that cos(2*theta) = cos(delta).cos(nu) and 
        // hphi2.cos(mu) - hphi3.sin(mu) = (tau/lambda).[cos(2*theta)-1.]
        // So we solve the following system where a=hphi2 and b=-hphi3 and c=(tau/lambda).[cos(2*theta)-1.]
        // ax + by = c
        // x² + y² = 1.
        double a = hphi[Y];
        double b =-hphi[Z];
        double c = k0*(cos_2theta-1.);
        double det = a*a+b*b-c*c;

        if (fabs(det) < constant::math::epsilon_1)
          det = 0.;
        if (det < -constant::math::epsilon_1)
          throw HKLException(
              "Unobtainable reflection",
              "Unreachable Bragg condition",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        double cos_mu = (a*c - b*sqrt(det))/(a*a+b*b);
        double sin_mu = (b*c + a*sqrt(det))/(a*a+b*b);
        if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+constant::math::epsilon_0) ||
            (cos_mu*cos_mu + sin_mu*sin_mu < 1.-constant::math::epsilon_0))
          throw HKLException(
              "Unobtainable reflection",
              "Mu circle cannot reach the diffraction position",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");
        if ((fabs(cos_mu) < constant::math::epsilon_1) && (fabs(sin_mu) < constant::math::epsilon_1))
          mu = 0.;
        else
          mu = atan2(sin_mu,cos_mu);
        smatrix MU;
        // Matrix Mu
        //  | 1.     0.       0.  |
        //  | 0.  cos_mu  -sin_mu |
        //  | 0.  sin_mu   cos_mu |
        MU.set(
            1.,    0.,      0.,
            0., cos_mu, -sin_mu,
            0., sin_mu,  cos_mu);

        // STEP NUMBER 2 : COMPUTING THE SCATTERING VECTOR Q
        // Q = MU.U.B.(h,k,l)
        svector Q = MU * hphi;

        // STEP NUMBER 3 : COMPUTING THE DIFFRACTED RAY Kf
        // Kf = Q + Ki where Ki = (tau/lambda).(0,1,0)
        svector Kf(Q[X], Q[Y]+k0, Q[Z]);

        // STEP NUMBER 4 : COMPUTING DELTA AND NU ORIENTATIONS
        // if (Kf)x > 0 then  0   < delta <  Pi
        // if (Kf)x < 0 then  Pi  < delta < 2Pi
        // if (Kf)y > 0 then -Pi/2< delta <  Pi/2
        // if (Kf)y < 0 then  Pi/2< delta < 3Pi/2
        // if (Kf)z > 0 then  0   <  nu   <  Pi
        // if (Kf)z > 0 then  Pi  <  nu   < 2Pi
        double sx = hphi[X] / k0;

        if (fabs(sx) > 1.+constant::math::epsilon_1)
          throw HKLException(
              "Unobtainable reflection, delta sine bigger than 1.",
              "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        delta = asin(sx);
        // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
        // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
        // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
        if (Kf[Y] < - constant::math::epsilon_1)
          delta = constant::math::pi - delta;

        double k02 = k0*k0;

        cos_delta = cos(delta);
        //double sin_delta = sin(delta);

        if (fabs(cos_delta) < constant::math::epsilon_1)
        {
          // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
          // not change the detector position as it is located on nu rotation axis.
          gamma = 0.;

          geometry.get_axe("mu").set_value(mu);
          geometry.get_axe("omega").set_value(omega);
          geometry.get_axe("chi").set_value(chi);
          geometry.get_axe("phi").set_value(phi);

          geometry.get_axe("gamma").set_value(gamma);
          geometry.get_axe("delta").set_value(delta);

          return;
        }

        double cc = (2*k02-hphi_length*hphi_length) / k02;
        cc = cc / (2*cos_delta);

        if (fabs(cc) > 1.)
          throw HKLException(
              "cos(nu) bigger than 1.",
              "cos(delta) may be small, the reflection is unreachable",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        gamma = acos(cc);
        if (Kf[Z] < -constant::math::epsilon_1)
          gamma = -gamma;

        geometry.get_axe("mu").set_value(mu);
        geometry.get_axe("omega").set_value(omega);
        geometry.get_axe("chi").set_value(chi);
        geometry.get_axe("phi").set_value(phi);

        geometry.get_axe("gamma").set_value(gamma);
        geometry.get_axe("delta").set_value(delta);

        return;


        /*
           double sx = hphi.get_X() / k0;

           if (fabs(sx) > 1.+mathematicalConstants::getEpsilon1())
           throw HKLException(
           "Unobtainable reflection, delta sine bigger than 1.",
           "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix",
           "eulerian_lifting3CDetectorMode6C::computeAngles()");

           delta = asin(sx);
        // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
        // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
        // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
        //if (hphi.get_Y() < -mathematicalConstants::getEpsilon1())
        //  delta = mathematicalConstants::getPI() - delta;

        double k02 = k0*k0;

        double cos_delta = cos(delta);
        //double sin_delta = sin(delta);

        if (fabs(cos_delta) < mathematicalConstants::getEpsilon1())
        {
        // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
        // not change the detector position as it is located on nu rotation axis.
        nu = 0.;

        double sin_mu = (hphi.get_Z()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
        double cos_mu =(-hphi.get_Y()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

        if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
        (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
        throw HKLException(
        "Unobtainable reflection",
        "Mu circle cannot reach the diffraction position",
        "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
        mu = 0.;
        else
        mu = atan2(sin_mu,cos_mu);

        ac6C = new angleConfiguration_Eulerian6C;
        ac6C->setDelta(delta);
        ac6C->setEta(eta);
        ac6C->setChi(chi);
        ac6C->setPhi(phi);
        ac6C->setNu(nu);
        ac6C->setMu(mu);

        return ac6C;
        }

        double cc = (2*k02-hphi_length*hphi_length) / k02;
        cc = cc / (2*cos_delta);

        if (fabs(cc) > 1.)
        throw HKLException(
        "cos(nu) bigger than 1.",
        "cos(delta) may be small, the reflection is unreachable",
        "eulerian_lifting3CDetectorMode6C::computeAngles()");

        nu = acos(cc);

        double cos_nu = cos(nu);
        double sin_nu = sin(nu);

        // We multiply by cos(chi) to "inject" its sign in s_phi and c_phi. 
        double sin_mu =-hphi.get_Z()*k0*(cos_delta*cos_nu-1.)+hphi.get_Y()*k0*sin_nu*cos_delta;
        double cos_mu = hphi.get_Y()*k0*(cos_delta*cos_nu-1.)+hphi.get_Z()*k0*sin_nu*cos_delta;
        sin_mu = sin_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
        cos_mu = cos_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

        if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
            (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
          throw HKLException(
              "Unobtainable reflection",
              "Mu circle cannot reach the diffraction position !",
              "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
          mu = 0.;
        else
          mu = atan2(sin_mu,cos_mu);

        ac6C = new angleConfiguration_Eulerian6C;
        ac6C->setDelta(delta);
        ac6C->setEta(eta);
        ac6C->setChi(chi);
        ac6C->setPhi(phi);
        ac6C->setNu(nu);
        ac6C->setMu(mu);

        return ac6C;
        */


          /*
          /////////////////////
          // Bragg relation //
          ///////////////////
          // sin(theta) = || q || * lambda * 0.5 / tau.
          double sin_theta = hphi_length * lambda * 0.5;
          // We have to be consistent with the conventions 
          // previously defined when we computed the crystal 
          // reciprocal lattice.
          sin_theta = sin_theta / constant::physic::tau;

          if (fabs(sin_theta) > 1.+mathematicalConstants::getEpsilon1())
          throw HKLException(
          "sine bigger than 1.",
          "hphi_length too big, maybe error in UB matrix",
          "eulerian_lifting3CDetectorMode6C::computeAngles()");

          double theta = asin(sin_theta);

          double a = hphi.get_Y();
          double b =-hphi.get_Z();
          double c = k0*(cos(2*theta)-1.);
          double sqrt1 = sqrt(a*a+b*b-c*c);
          double cos_mu = (a*c-b*sqrt1)/(a*a+b*b);
          double sin_mu = (b*c+a*sqrt1)/(a*a+b*b);

          // Another solution.
          //double cos_mu = (a*c+b*sqrt1)/(a*a+b*b);
          //double sin_mu = (b*c-a*sqrt1)/(a*a+b*b);

          if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
          mu = 0.;
          else
          mu = atan2(sin_mu,cos_mu);





          double sx = hphi.get_X() / k0;

          if (fabs(sx) > 1.+mathematicalConstants::getEpsilon1())
          throw HKLException(
          "Unobtainable reflection, delta sine bigger than 1.",
          "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix",
          "eulerian_lifting3CDetectorMode6C::computeAngles()");

          delta = asin(sx);
          // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
          // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
          // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
          //if (hphi.get_Y() < -mathematicalConstants::getEpsilon1())
          //  delta = mathematicalConstants::getPI() - delta;

          double k02 = k0*k0;

          double cos_delta = cos(delta);
          double sin_delta = sin(delta);

          if (fabs(cos_delta) < mathematicalConstants::getEpsilon1())
          {
          // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
          // not change the detector position as it is located on nu rotation axis.
          nu = 0.;

          double sin_mu = (hphi.get_Z()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
          double cos_mu =(-hphi.get_Y()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

          if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
          (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
          throw HKLException(
          "Unobtainable reflection",
          "Mu circle cannot reach the diffraction position",
          "eulerian_lifting3CDetectorMode6C::computeAngles()");

        if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
          mu = 0.;
        else
          mu = atan2(sin_mu,cos_mu);

        ac6C = new angleConfiguration_Eulerian6C;
        ac6C->setDelta(delta);
        ac6C->setEta(eta);
        ac6C->setChi(chi);
        ac6C->setPhi(phi);
        ac6C->setNu(nu);
        ac6C->setMu(mu);

        return ac6C;
      }

      double cc = (2*k02-hphi_length*hphi_length) / k02;
      cc = cc / (2*cos_delta);

      if (fabs(cc) > 1.)
        throw HKLException(
            "cos(nu) bigger than 1.",
            "cos(delta) may be small, the reflection is unreachable",
            "eulerian_lifting3CDetectorMode6C::computeAngles()");

      nu = acos(cc);

      double cos_nu = cos(nu);
      double sin_nu = sin(nu);

      // We multiply by cos(chi) to "inject" its sign in s_phi and c_phi. 
      double sin_mu =-hphi.get_Z()*k0*(cos_delta*cos_nu-1.)+hphi.get_Y()*k0*sin_nu*cos_delta;
      double cos_mu = hphi.get_Y()*k0*(cos_delta*cos_nu-1.)+hphi.get_Z()*k0*sin_nu*cos_delta;
      sin_mu = sin_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
      cos_mu = cos_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

      if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
          (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
        throw HKLException(
            "Unobtainable reflection",
            "Mu circle cannot reach the diffraction position !",
            "eulerian_lifting3CDetectorMode6C::computeAngles()");

      if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
        mu = 0.;
      else
        mu = atan2(sin_mu,cos_mu);

      ac6C = new angleConfiguration_Eulerian6C;
      ac6C->setDelta(delta);
      ac6C->setEta(eta);
      ac6C->setChi(chi);
      ac6C->setPhi(phi);
      ac6C->setNu(nu);
      ac6C->setMu(mu);

      return ac6C;
      */
      }

    } // namesapce eulerian6C
  } // name space mode
} // namespace hkl
