//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/svector.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class svector

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.6 $

//

// $Log: svector.cpp,v $
// Revision 1.6  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.5.2.15  2005/08/29 07:53:37  picca
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
// Revision 1.5.2.14  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.5.2.13  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.5.2.12  2005/06/23 16:29:04  picca
// Juste avant le depart pour montreal.
//
// Revision 1.5.2.11  2005/06/20 07:08:10  picca
// affinement suite
//
// Revision 1.5.2.10  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.5.2.9  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.5.2.8  2005/03/31 16:25:51  picca
// la suite
//
// Revision 1.5.2.7  2005/03/31 15:57:57  picca
// changements dans les classes: svector, quaternion, reflection
// - ajout des methodes getSampleRotationMatrix et getQ Ã  la classe reflection
//
// Revision 1.5.2.6  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.5.2.5  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.5.2.4  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply
//
// Revision 1.5.2.3  2005/03/10 16:18:55  picca
// -typo
// - remove the printOnScreen function of svector and smatrix
//
// Revision 1.5.2.2  2005/03/09 17:40:29  picca
// modification de == pour que l'erreur de précision soit prise en compte
//
// Revision 1.5.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.5  2005/02/08 15:51:05  picca
// update the documenattion
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
#include "svecmat.h"

namespace hkl {

//!< Default constructor.
svector::svector(): 
  std::valarray<double>(0., 3)
{}

//!< Constructor to allocate a 3D vector and populate it with data.
svector::svector(double const & a, double const & b, double const & c) :
  std::valarray<double>(0., 3)
{
  (*this)[0] = a;
  (*this)[1] = b;
  (*this)[2] = c;
}

// Copy constructor.
svector::svector(svector const & v) :
  std::valarray<double>(v)
{}

bool
svector::operator ==(svector const & v) const
{
  unsigned int i;

  for(i=0; i<3; i++)
    if (fabs((*this)[i]-v[i]) > constant::math::epsilon_1)
      return false;

  return true;
}

svector &
svector::operator*= (smatrix const & M)
{
  svector u(*this);

  (*this)[X] = u[X] * M.m_mat11 + u[Y] * M.m_mat21 + u[Z] * M.m_mat31;
  (*this)[Y] = u[X] * M.m_mat12 + u[Y] * M.m_mat22 + u[Z] * M.m_mat32;
  (*this)[Z] = u[X] * M.m_mat13 + u[Y] * M.m_mat23 + u[Z] * M.m_mat33;

  return *this;
}

svector &
svector::operator*= (double const & d)
{
  (*this)[X] *= d;
  (*this)[Y] *= d;
  (*this)[Z] *= d;
  
  return *this;
}

void
svector::set(double const & a, double const & b, double const & c)
{
  (*this)[X] = a;
  (*this)[Y] = b;
  (*this)[Z] = c;
}

// Scalar product.
double
svector::scalar(svector const & u) const
{
  return (*this)[X] * u[X] + (*this)[Y] * u[Y] + (*this)[Z] * u[Z];
}

svector
svector::vectorialProduct(svector const & v) const
{
  svector z;
  
  z[X] = (*this)[Y] * v[Z] - (*this)[Z] * v[Y];
  z[Y] = (*this)[Z] * v[X] - (*this)[X] * v[Z];
  z[Z] = (*this)[X] * v[Y] - (*this)[Y] * v[X];
  
  return z;
}

double
svector::angle(svector const & v) const
{
  double norm_v = v.norm2();
  double norm_this = norm2();
  double norm = norm_v * norm_this;
  
  double cosine = scalar(v) / norm;

  return acos(cosine);
}

smatrix
svector::axisSystem(svector const & v) const
{
  smatrix M;
  
  svector XX = normalize();
  svector ZZ = vectorialProduct(v).normalize();
  svector YY = ZZ.vectorialProduct(XX);

  M.set(
    XX[X], YY[X], ZZ[X],
    XX[Y], YY[Y], ZZ[Y],
    XX[Z], YY[Z], ZZ[Z]);

  return M;
}
// Vector length.
double
svector::norm2(void) const
{
  return sqrt((*this)[X] * (*this)[X] + (*this)[Y] * (*this)[Y] + (*this)[Z] * (*this)[Z]);
}

// Infinite norm.
double
svector::norminf(void) const
{
  double t = 0.0;

  if (fabs((*this)[X]) > fabs((*this)[Y]))
    t = fabs((*this)[X]);
  else
    t = fabs((*this)[Y]);
  if (fabs((*this)[Z]) > t)
    t = fabs((*this)[Z]);
  return t;
}

svector
svector::normalize(void) const
{
  double norm = this->norm2();
  return svector((*this)[X] / norm, (*this)[Y] / norm, (*this)[Z] / norm);
}

void
svector::randomize(void)
{
  unsigned int i;

  for(i=0;i<3;i++){
    #ifdef VCPP6
      (*this)[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
    #else
      (*this)[i] = -1 + 2 * std::rand()/(RAND_MAX+1.0);
    #endif  
  }
}

} //namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::svector const & v)
{
  flux << "<" << v[X] << ", " << v[Y] << ", " << v[Z] << ">";
  return flux;
}

