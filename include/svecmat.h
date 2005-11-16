//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/svecmat.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class svector, smatrix

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.3 $

//

// $Log: svecmat.h,v $
// Revision 1.3  2005/11/16 12:42:49  picca
// * modified crystal::randomize to deal with different combination of alpha, beta and gamma fit.
//
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
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
// Revision 1.1.2.13  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.1.2.12  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.11  2005/06/23 16:29:04  picca
// Juste avant le depart pour montreal.
//
// Revision 1.1.2.10  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.1.2.9  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.8  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.1.2.7  2005/03/31 15:57:57  picca
// changements dans les classes: svector, quaternion, reflection
// - ajout des methodes getSampleRotationMatrix et getQ Ã  la classe reflection
//
// Revision 1.1.2.6  2005/03/31 14:30:42  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.5  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.1.2.4  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply
//
// Revision 1.1.2.3  2005/03/10 16:18:55  picca
// -typo
// - remove the printOnScreen function of svector and smatrix
//
// Revision 1.1.2.2  2005/03/09 17:40:29  picca
// modification de == pour que l'erreur de précision soit prise en compte
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
/// File svecmat.h

#ifndef _SVECMAT_H_
#define _SVECMAT_H_

#include <math.h>
#include <iostream>
#include <valarray>
#include <cstdlib>
#include <cstdarg>

#include "config.h"
#include "HKLException.h"
#include "constants.h"

#define X 0
#define Y 1
#define Z 2

#ifdef VCPP6
  #define std::rand rand
#endif

using std::vector;

namespace hkl {

  class smatrix;

  /// Define a vector in a three dimensionnal space.
  class svector : public std::valarray<double>
  {
    friend class smatrix;

  public:
    /**
     * \brief Default constructor
     * 
     * Create a new vector and set all its components to 0.0
     */
    svector(void);
    
    /**
     * \brief This constructor creates a 3D vector and populates it
     * \param[in] el1
     * \param[in] el2
     * \param[in] el3
     * 
     * Create a new vector with el1, el2 and el3 as coordinates
     */
    svector(double const & el1,double const & el2,double const & el3);

    /**
     * \brief Copy constructor.
     * \param v #svector
     * 
     * Create a new vector by copying the v vector.
     */
    svector(svector const & v);

    /**
     * \brief Compare two svector
     * @param v 
     * @return 1 if this == Y, 0 otherwise
     */
    bool operator == (svector const & v) const;

    /** 
     * \brief Right multiplication with a smatrix
     */
    svector & operator *= (smatrix const & M);

    /**
     * \brief Right multiplication with a double
     */
     svector & operator *= (double const & d);
     
    /**
     * \brief Set the elements of the svector with the a,b and c parameters
     * @param a 
     * @param b 
     * @param c 
     */
    void set(double const & a, double const & b, double const & c);

    /**
     * \brief Scalar product.
     * \param[in] u #svector
     * \return The scalar product of \f$\vec{this}\f$ by \f$\vec{u}\f$
     */
    double scalar(svector const & u) const;

    /**
     * \brief Vectorial product : \f$ \vec{Z} = \vec{this} \times \vec{Y}\f$.
     * \param[in] v #svector
     * \return  Z #svector
     * 
     * This method computes the vectorial product with Y and 
     * it puts the result into Z.
     */
    svector vectorialProduct(svector const & v) const;

    /**
     * @brief Compute the angle between 2 vectors
     * @return the angle in radian
     */
    double angle(svector const & v) const;

    /**
     * \brief Creation of an axis system with unit vectors.
     * \param v #svector
     * \return  M #smatrix
     * 
     * This method creates a direct axis system M with the Y vector.
     * All the M vectors are units vectors.
     * \f[ M = ( \vec{v1}, \vec{v2}, \vec{v3}) \f]
     * where
     * \f[
     *  \begin{aligned}
     *    \vec{v1} & = \frac{\vec{this}}{||\vec{this}||_2} \\
     *    \vec{v2} & = \frac{\vec{Y}}{||\vec{Y}||_2} \\
     *    \vec{v3} & = \vec{v1} \times \vec{v2} \\
     *  \end{aligned}
     * \f]
     */
    smatrix axisSystem(svector const & v) const;

    /**
     * \brief Return the Euclidean norm of the vector.
     * \return The euclidean norm of the vector
     * 
     * This method computes the Euclidean norm of the vector \f$ \vec{this}=(m\_v1, m\_v2, m\_v3) \f$.
     * \f[
     * ||\vec{this}||_2 = \sqrt{m\_v1^2+m\_v2^2+m\_v3^2}
     * \f]
     */
    double norm2(void) const;

    /**
     * \brief Return the infinit norm of the vector.
     * \return The infinit norm of the vector
     * 
     * This method computes the infinit norm of the vector \f$ \vec{this}=(m\_v1, m\_v2, m\_v3) \f$.
     * \f[
     *   ||\vec{this}||_\infty = \max(|m\_v1|,|m\_v2|,|m\_v3|)
     * \f]
     */
    double norminf(void) const;

    /**
     * \brief normalize the vector
     * \return The normalize vector
     */
    svector normalize(void) const;

    /**
     * @brief Return a #svector with element randomly chosen between -1 and 1
     */
    void randomize(void);

    /**
     * @brief Return a #svector with element randomly chosen between -1 and 1
     */
    svector & randomize(svector const & v);
    svector & randomize(svector const & v1, svector const & v2);

    /**
     * \brief rotate a vector around another one with an angle.
     * \param axe The svector corresponding to the rotation axe.
     * \param angle the angle of rotation.
     * \return The new vector.
     */
    svector rotatedAroundVector(svector const & axe, double const & angle) const;
  };

} // namespace hkl

 /**
   * \brief Surcharge de l'operateur << pour la class svector
   * @param flux 
   * @param v 
   * @return 
   */
std::ostream & operator << (std::ostream & flux, hkl::svector const & v);

namespace hkl {

  /// Define a matrix in a three dimensionnal space.
  class smatrix
  {
    friend class svector;

  private:
    double m_mat11; /**< element 11 of the matrix*/
    double m_mat12; /**< element 12 of the matrix*/
    double m_mat13; /**< element 13 of the matrix*/
    double m_mat21; /**< element 21 of the matrix*/
    double m_mat22; /**< element 22 of the matrix*/
    double m_mat23; /**< element 23 of the matrix*/
    double m_mat31; /**< element 31 of the matrix*/
    double m_mat32; /**< element 32 of the matrix*/
    double m_mat33; /**< element 33 of the matrix*/

  public:
    /**
     * \brief Default constructor
     * 
     * Create a new identity matrix.
     */
    smatrix(void);

    /**
     * \brief This constructor creates a 3*3 matrix and populates it.
     * \param el11
     * \param el12
     * \param el13
     * \param el21
     * \param el22
     * \param el23
     * \param el31
     * \param el32
     * \param el33
     *
     *
     * Create a new matrix from the elij values.
     */
    smatrix( double el11, double el12, double el13,
             double el21, double el22, double el23,
             double el31, double el32, double el33);

    /**
     * @brief Creat a new matrix from the 3 eulerian angles
     * @param euler_x
     * @param euler_y
     * @param euler_z
     */
    smatrix( double euler_x, double euler_y, double euler_z );

    /**
     * \brief Copy constructor.
     * \param M The original matrix
     *
     * Create a new matrix by copying the elements of the matrix \b M
     */
    smatrix(smatrix const & M);

    /**
     * \brief Test if this is equal to the smatrix M 
     * @param M the smatrix to compare with
     * @return 1 if the two smatrix are equal, 0 otherwise
     */
    bool operator == (smatrix const & M) const;
    
    /** 
     * \brief Right multiplication with a smatrix
     */
    smatrix & operator *= (smatrix const & M);
    
    /** 
     * \brief Right multiplication with a smatrix
     * \return a svector
     */
    smatrix operator * (smatrix const & M) const;
    
    /** 
     * \brief Right multiplication with a svector
     * \return a svector
     */
    svector operator * (svector const & v) const;
   
    /**
     * \brief Copy a matrix.
     * \param M the original matrix
     *
     * Set the elements of the matrix \b this using the elements of the matrix \b M
     */
    void set(smatrix const & M);

    /**
     * \brief Give the fields a new value.
     * \param  el11
     * \param  el12
     * \param  el13
     * \param  el21
     * \param  el22
     * \param  el23
     * \param  el31
     * \param  el32
     * \param  el33
     *
     * set the fields of the matrix \b this with the elements elij
     */
    void set( double el11, double el12, double el13,
              double el21, double el22, double el23,
              double el31, double el32, double el33);

    /**
     * \brief Get a matrix element.
     * \return The value of the i,j element of the matrix \b this
     */
    double get(int i,int j) const throw (HKLException);

    /**
     * \brief Transposition.
     *
     * Transpose the matrice \b this
     */
    smatrix & transpose(void);
    
    /**
     * \brief Return the matrix as an eulerian representation.
     * \return an #svector with the 3 angles.
     */
    svector asEulerian(void) const;
  };

} // namespace hkl

/**
   * \brief Surcharge de l'operateur << pour la class smatrix
   * @param flux 
   * @param m 
   * @return 
   */
std::ostream & operator << (std::ostream & flux, hkl::smatrix const & m);

#endif // _SVECMAT_H_
