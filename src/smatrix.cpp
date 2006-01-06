//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/smatrix.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class smatrix

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.8 $

//

// $Log: smatrix.cpp,v $
// Revision 1.8  2006/01/06 16:24:30  picca
// * modification of the bksys files
//
// Revision 1.7  2005/12/13 09:53:53  picca
// * fir windows test compile.
//
// Revision 1.6  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.5.2.12  2005/08/29 07:53:37  picca
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
// Revision 1.5.2.11  2005/06/23 16:29:04  picca
// Juste avant le depart pour montreal.
//
// Revision 1.5.2.10  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.5.2.9  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.5.2.8  2005/05/26 14:36:54  picca
// class diffractometer
// - ajout getLattice, setLattice, getReciprocalLattice
// -ajout getCrystalLattice, setCrystalLattice, getCrystalReciprocalLattice
// -ajout des test correspondants.
// -ajout getReflection, getCrystalReflection
// -ajout d'un binding pour python de la classe diffractometer qui utilise la librairie boost_python
// -ajout d'un GUI en python + PyGtk
//
// Revision 1.5.2.7  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.5.2.6  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.5.2.5  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.5.2.4  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply
//
// Revision 1.5.2.3  2005/03/10 16:18:55  picca
// -typo
// - remove the printOnScreen function of svector and smatrix
//
// Revision 1.5.2.2  2005/03/09 17:17:12  picca
// modification de == pour prendre en compte les problèmes de précision
//
// Revision 1.5.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.5  2005/01/27 09:23:53  delos
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
  
  // Default constructor.
  smatrix::smatrix(void)
  {
    m_mat11 = 0.;
    m_mat12 = 0.;
    m_mat13 = 0.;
    m_mat21 = 0.;
    m_mat22 = 0.;
    m_mat23 = 0.;
    m_mat31 = 0.;
    m_mat32 = 0.;
    m_mat33 = 0.;
  }
  
  // Constructor to allocate a 3D matrix and populate it with data.
  smatrix::smatrix(double e11, double e12, double e13,
                   double e21, double e22, double e23,
                   double e31, double e32, double e33)
  {
    m_mat11 = e11;
    m_mat12 = e12;
    m_mat13 = e13;
    m_mat21 = e21;
    m_mat22 = e22;
    m_mat23 = e23;
    m_mat31 = e31;
    m_mat32 = e32;
    m_mat33 = e33;
  }
  
  smatrix::smatrix(double euler_x, double euler_y, double euler_z)
  {
    double A = cos(euler_x);
    double B = sin(euler_x);
    double C = cos(euler_y);
    double D = sin(euler_y);
    double E = cos(euler_z);
    double F = sin(euler_z);
    double AD = A * D;
    double BD = B * D;
   
    m_mat11 = C*E;
    m_mat12 =-C*F;
    m_mat13 = D;
    m_mat21 = BD * E + A * F;
    m_mat22 =-BD * F + A * E;
    m_mat23 =-B * C;
    m_mat31 =-AD * E + B * F;
    m_mat32 = AD * F + B * E;
    m_mat33 = A * C;
  }
  
  // Copy constructor.
  smatrix::smatrix(smatrix const & M)
  {
    m_mat11 = M.m_mat11;
    m_mat12 = M.m_mat12;
    m_mat13 = M.m_mat13;
    m_mat21 = M.m_mat21;
    m_mat22 = M.m_mat22;
    m_mat23 = M.m_mat23;
    m_mat31 = M.m_mat31;
    m_mat32 = M.m_mat32;
    m_mat33 = M.m_mat33;
  }
  
  bool
  smatrix::operator ==(smatrix const & M) const
  {
    if (fabs(m_mat11 - M.m_mat11) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat12 - M.m_mat12) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat13 - M.m_mat13) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat21 - M.m_mat21) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat22 - M.m_mat22) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat23 - M.m_mat23) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat31 - M.m_mat31) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat32 - M.m_mat32) > constant::math::epsilon_1)
      return false;
    if (fabs(m_mat33 - M.m_mat33) > constant::math::epsilon_1)
      return false;
  
    return true;
  }
  
  smatrix &
  smatrix::operator *= (smatrix const & M)
  {
    smatrix M1(*this);
    smatrix M2 = M;
  
    m_mat11 = M1.m_mat11 * M2.m_mat11 + M1.m_mat12 * M2.m_mat21 + M1.m_mat13 * M2.m_mat31;
    m_mat12 = M1.m_mat11 * M2.m_mat12 + M1.m_mat12 * M2.m_mat22 + M1.m_mat13 * M2.m_mat32;
    m_mat13 = M1.m_mat11 * M2.m_mat13 + M1.m_mat12 * M2.m_mat23 + M1.m_mat13 * M2.m_mat33;
    m_mat21 = M1.m_mat21 * M2.m_mat11 + M1.m_mat22 * M2.m_mat21 + M1.m_mat23 * M2.m_mat31;
    m_mat22 = M1.m_mat21 * M2.m_mat12 + M1.m_mat22 * M2.m_mat22 + M1.m_mat23 * M2.m_mat32;
    m_mat23 = M1.m_mat21 * M2.m_mat13 + M1.m_mat22 * M2.m_mat23 + M1.m_mat23 * M2.m_mat33;
    m_mat31 = M1.m_mat31 * M2.m_mat11 + M1.m_mat32 * M2.m_mat21 + M1.m_mat33 * M2.m_mat31;
    m_mat32 = M1.m_mat31 * M2.m_mat12 + M1.m_mat32 * M2.m_mat22 + M1.m_mat33 * M2.m_mat32;
    m_mat33 = M1.m_mat31 * M2.m_mat13 + M1.m_mat32 * M2.m_mat23 + M1.m_mat33 * M2.m_mat33;
  
    return *this;
  }
  
  smatrix
  smatrix::operator * (smatrix const & M2) const
  {
    smatrix result(*this);
  
    result *= M2;
  
    return result;
  }
  
  svector
  smatrix::operator *(svector const & v) const
  {
    svector result;
  
    result[0] = v[0] * m_mat11 + v[1] * m_mat12 + v[2] * m_mat13;
    result[1] = v[0] * m_mat21 + v[1] * m_mat22 + v[2] * m_mat23;
    result[2] = v[0] * m_mat31 + v[1] * m_mat32 + v[2] * m_mat33;
  
    return result;
  }
  
  
  void
  smatrix::set(smatrix const & M)
  {
    m_mat11 = M.m_mat11;
    m_mat12 = M.m_mat12;
    m_mat13 = M.m_mat13;
    m_mat21 = M.m_mat21;
    m_mat22 = M.m_mat22;
    m_mat23 = M.m_mat23;
    m_mat31 = M.m_mat31;
    m_mat32 = M.m_mat32;
    m_mat33 = M.m_mat33;
  }
  
  // Set all the fields.
  void
  smatrix::set(double el11, double el12, double el13,
               double el21, double el22, double el23,
               double el31, double el32, double el33)
  {
    m_mat11 = el11;
    m_mat12 = el12;
    m_mat13 = el13;
    m_mat21 = el21;
    m_mat22 = el22;
    m_mat23 = el23;
    m_mat31 = el31;
    m_mat32 = el32;
    m_mat33 = el33;
  }
  
  // Transposition.
  smatrix &
  smatrix::transpose(void)
  {
    smatrix M(*this);
  
    m_mat11 = M.m_mat11;
    m_mat12 = M.m_mat21;
    m_mat13 = M.m_mat31;
    m_mat21 = M.m_mat12;
    m_mat22 = M.m_mat22;
    m_mat23 = M.m_mat32;
    m_mat31 = M.m_mat13;
    m_mat32 = M.m_mat23;
    m_mat33 = M.m_mat33;
    
    return (*this);
  }
  
  double
  smatrix::get(int i, int j) const throw (HKLException)
  {
    if (i==0 && j==0)
      return m_mat11;
    if (i==1 && j==0)
      return m_mat21;
    if (i==2 && j==0)
      return m_mat31;
  
    if (i==0 && j==1)
      return m_mat12;
    if (i==1 && j==1)
      return m_mat22;
    if (i==2 && j==1)
      return m_mat32;
  
    if (i==0 && j==2)
      return m_mat13;
    if (i==1 && j==2)
      return m_mat23;
    if (i==2 && j==2)
      return m_mat33;
    else
      throw HKLException(
        "Unable to get such an element",
        "i>=3 or i<0 or j>=3 or j<0",
        "smatrix::get()");
  }
  
  svector
  smatrix::asEulerian(void) const
  {
    svector eulerian;
    double angle_x, angle_y, angle_z;
    double tx, ty;
    
    angle_y = asin( m_mat13 );        /* Calculate Y-axis angle */
    double C = cos( angle_y );
    if (fabs(C) > constant::math::epsilon_0){             /* Gimball lock? */
      tx =  m_mat33 / C;           /* No, so get X-axis angle */
      ty = -m_mat23 / C;
      angle_x = atan2( ty, tx );
      tx =  m_mat11 / C;            /* Get Z-axis angle */
      ty = -m_mat12 / C;
      angle_z = atan2( ty, tx );
    }else{                               /* Gimball lock has occurred */
      angle_x  = 0.;                      /* Set X-axis angle to zero */
      tx      =  m_mat22;                 /* And calculate Z-axis angle */
      ty      =  m_mat21;
      angle_z  = atan2( ty, tx );
    }
    eulerian.set(angle_x, angle_y, angle_z);
    
    return eulerian;
  }

  ostream &
  smatrix::toStream(ostream & flux) const
  {
    flux << setprecision(constant::math::precision);
    flux << " " << m_mat11 << " " << m_mat12 << " " << m_mat13;
    flux << " " << m_mat21 << " " << m_mat22 << " " << m_mat23;
    flux << " " << m_mat31 << " " << m_mat32 << " " << m_mat33 << endl;    
    return flux;    
  }

  istream &
  smatrix::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision);
    flux >> m_mat11 >> m_mat12 >> m_mat13;
    flux >> m_mat21 >> m_mat22 >> m_mat23;
    flux >> m_mat31 >> m_mat32 >> m_mat33;
    return flux;
  }
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::smatrix const & m)
{
  flux << endl;
  flux << showpoint << showpos;
  flux << m.get(0,0) << '\t' << m.get(0,1) << '\t' << m.get(0,2) << endl;
  flux << m.get(1,0) << '\t' << m.get(1,1) << '\t' << m.get(1,2) << endl;
  flux << m.get(2,0) << '\t' << m.get(2,1) << '\t' << m.get(2,2) << endl;
  flux << noshowpoint << noshowpos << dec;
  return flux;
}
