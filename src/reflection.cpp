//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/reflection.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class reflection

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.9 $

//

// $Log: reflection.cpp,v $
// Revision 1.9  2005/11/14 13:34:14  picca
// * update the Simplex method.
//
// Revision 1.8  2005/10/26 15:11:41  picca
// * AngleConfiguration -> Geometry
// * add PseudoAxe class
//
// Revision 1.7  2005/10/20 12:48:47  picca
// * right calculation for the number of usable reflections
// close: #976 #977
//
// Revision 1.6.2.1  2005/10/20 12:40:20  picca
// * modification of Geometry::getAxesNames()
// * add Reflection::isColinear() + test functions
// * add Crystal::isEnoughReflections() + test functions
// * remove crystal::getNumberOfReflectionsForCalculation() what a silly name :)
// * close #976 #977
//
// Revision 1.6  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.5.2.22  2005/08/29 07:53:37  picca
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
// Revision 1.5.2.21  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.5.2.20  2005/06/22 15:04:36  picca
// surcharge de operator[] pour la classe Geometry
//
// Revision 1.5.2.19  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.5.2.18  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.5.2.17  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.5.2.16  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.5.2.15  2005/05/27 12:30:34  picca
// class: Reflection
// 	- ajout contructeur par default
// 	- ajout set(const Reflection & reflection) pour mettre Ã  jour un reflection Ã  partir d'une autre
// 	- ajout get_angleConfiguration et set_valueConfiguration
// 	- ajout get_source, set_source
// 	- remplacement getRelevance par get_relevance
// 	- remplacement getFlag par get_flag
//
// Revision 1.5.2.14  2005/05/26 14:36:54  picca
// class diffractometer
// - ajout getLattice, setLattice, getReciprocalLattice
// -ajout getCrystalLattice, setCrystalLattice, getCrystalReciprocalLattice
// -ajout des test correspondants.
// -ajout getReflection, getCrystalReflection
// -ajout d'un binding pour python de la classe diffractometer qui utilise la librairie boost_python
// -ajout d'un GUI en python + PyGtk
//
// Revision 1.5.2.13  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.5.2.12  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.5.2.11  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.5.2.10  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.5.2.9  2005/03/31 11:28:26  picca
// Modification de la classe Reflection.
// - ajout de 2 champs:
// 	m_source pour sauvegarder l'Ã©tat de la source pour chaque reflection.
// 	m_flag pour indiquer si oui ou non on utilise la reflection dans le calcule de U
// - ajout des getSet pour tous les champs de reflection.
// - ajout des test de ces getSet.
//
// Revision 1.5.2.8  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.5.2.7  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.5.2.6  2005/03/10 15:20:01  picca
// -Ajout dans operateur == du test sur m_setOfAngles
//
// Revision 1.5.2.5  2005/03/10 10:00:15  picca
// -add a static string m_strRelevance to display the relevance in a human readable way.
//
// modified the operator<<  to display the string and no mode int reflection::relevance
//
// Revision 1.5.2.4  2005/03/03 09:17:33  picca
// Ajout de la surcharge de l'operateur<< pour la classe reflection.
// Ajout de la surcharge de l'opérateur== pour la classe reflection. On ne prend pas encore en compte la comparaison de la classe angleconfiguration qui est à revoir.
// Suppression de la la fonction printOnScreen remplacée par la première surcharge.
//
// Revision 1.5.2.3  2005/03/02 12:43:57  picca
// add the operator<< for the anglecalculation classes.
//
// Revision 1.5.2.2  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.5.2.1  2005/03/02 09:20:23  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.5  2005/02/08 17:03:08  picca
// update the documentation
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
#include "reflection.h"

namespace hkl {

  Reflection::Reflection(void) {}

  Reflection::Reflection(Reflection const & reflection)
  {
    m_geometry = reflection.m_geometry;
    m_relevance = reflection.m_relevance;
    m_h = reflection.m_h;
    m_k = reflection.m_k;
    m_l = reflection.m_l;
    m_flag = reflection.m_flag;
    m_hkl_phi = reflection.m_hkl_phi;
  }

  Reflection::Reflection(Geometry const & geometry,
                         double const & h,
                         double const & k,
                         double const & l,
                         int const & relevance,
                         bool const & flag)
  {
    m_geometry = geometry;
    m_relevance = relevance;
    m_h = h;
    m_k = k;
    m_l = l;
    m_flag = flag;
    
    // do not forgot to update m_hkl_phi
    m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
  }

  Reflection::~Reflection(void) {}

  bool
  Reflection::operator == (Reflection const & reflection) const
  {
    return m_geometry == reflection.m_geometry
            && m_h == reflection.m_h
            && m_k == reflection.m_k
            && m_l == reflection.m_l
            && m_relevance == reflection.m_relevance
            && m_flag == reflection.m_flag
            && m_hkl_phi == reflection.m_hkl_phi;
  }


  void
  Reflection::set_geometry(Geometry const & geometry)
  {
    m_geometry = geometry;

    // do not forgot to update m_hkl_phi
    m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
  }

  std::string
  Reflection::getStrRelevance(void) const
  {
    return m_strRelevance[m_relevance];
  }

  bool
  Reflection::toggle(void)
  {
    m_flag = !m_flag;
    
    return m_flag;
  }

  svector
  Reflection::getHKL(void) const
  {
    return svector(m_h, m_k, m_l);
  }

  double
  Reflection::computeAngle(double const & h, double const & k, double const & l) const
  {
    double dot_product = h * m_h + k * m_k + l * m_l;
    double length1 = sqrt(m_h*m_h + m_k*m_k + m_l*m_l);
    double length2 = sqrt(h*h + k*k + l*l);
    double cosine = dot_product / (length1*length2);

    return acos(cosine);
  }

  bool
  Reflection::isColinear(Reflection const & reflection) const
  {
    svector v1(m_h, m_k, m_l);
    svector v2(reflection.m_h, reflection.m_k, reflection.m_l);
    if ((v1.vectorialProduct(v2)).norm2() < constant::math::epsilon_1)
      return true;
    else
      return false;
  }

  std::string
  Reflection::m_strRelevance[] = {"notVerySignificant", "Significant", "VerySignificant", "Best"};

} // namespace hkl

std::ostream &
operator << (std::ostream & flux, hkl::Reflection const & reflection)
{
  flux << "reflection: "
    << "hkl = "
      << reflection.get_h() << ", "
      << reflection.get_k() << ", "
      << reflection.get_l() << ", "
    << "relevance = " << reflection.getStrRelevance()
    << "(flag:" << reflection.get_flag() << ") "
    << std::endl
    << reflection.get_geometry();
  
  return flux;
}
