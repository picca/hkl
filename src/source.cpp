//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/source.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class source

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.6 $

//

// $Log: source.cpp,v $
// Revision 1.6  2005/12/05 10:34:43  picca
// * When adding a reflection with the same (hkl) than another one, the flag is
//   automatically set to false.
//
// Revision 1.5  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.4.2.9  2005/08/29 07:53:37  picca
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
// Revision 1.4.2.8  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.4.2.7  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.4.2.6  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.4.2.5  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.4.2.4  2005/03/31 16:25:51  picca
// la suite
//
// Revision 1.4.2.3  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.4.2.2  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.4.2.1  2005/03/02 08:12:21  picca
// Ajout de:
// operator<< et operator==
// suppression de printOnScreen
//
// Revision 1.4  2005/02/08 15:51:05  picca
// update the documenattion
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
#include "source.h"

namespace hkl {

Source::Source(void)
{
  m_waveLength = 0.;
  m_direction = svector(1., 0., 0.);
  m_qi = Quaternion(0., 1., 0., 0.);
}

Source::Source(Source const & source)
{
  m_waveLength = source.m_waveLength;
  m_direction = source.m_direction;
  m_qi = source.m_qi;
}

Source::Source(double const & waveLength, svector const & direction)
{
  m_waveLength = waveLength;
  m_direction = direction.normalize();

  double k = constant::physic::tau / m_waveLength;
  svector ki(m_direction);
  ki *= k;
  m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
}

bool
Source::operator ==(Source const & source) const
{
	return m_waveLength == source.m_waveLength
          && m_direction == source.m_direction
          && m_qi == source.m_qi;
}

void
Source::setWaveLength(double waveLength) throw (HKLException)
{
  if (fabs(waveLength) < constant::math::epsilon_0)
    throw HKLException("waveLength == 0",
                       "Please set a non zero wave length",
                       "Source::setWaveLength");
  
  m_waveLength = waveLength;
  
  double k = constant::physic::tau / m_waveLength;
  svector ki(m_direction);
  ki *= k;
  m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
}

void
Source::setDirection(svector const & direction)
{
  m_direction = direction.normalize();
  
  double k = constant::physic::tau / m_waveLength;
  svector ki(m_direction);
  ki *= k;
  m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
}

svector
Source::getKi(void) const
{
  double k = constant::physic::tau / m_waveLength;
  
  svector ki(m_direction);
  ki *= k;

  return ki;
}

void
Source::setKi(svector const & ki)
{
  m_waveLength = constant::physic::tau / ki.norm2();
  m_direction = ki.normalize();

  m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
}

} // namespace hkl

std::ostream &
operator << (std::ostream& flux, hkl::Source const & source)
{
  flux << "Source: "
    << "Wave length = " << source.get_waveLength() << ", "
    << "Direction = " << source.get_direction() << ", "
    << "Qi = " << source.get_qi();
  
  return flux;
}
