//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/svecmat.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class svector, smatrix

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.10 $

//

// $Log: svecmat.h,v $
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

#ifndef VECMAT
#define VECMAT

#include "HKLException.h"

class smatrix;

/// Define a vector in a three dimensionnal space.
class svector
{
private:
  double m_v1; /**< 1st vector coordinate*/
  double m_v2; /**< 2nd vector coordinate*/
  double m_v3; /**< 3rd vector coordinate*/

public:
  /**
   * \brief Default constructor
   * 
   * Create a new vector and set all its components to 0.0
   */
  svector();
  
  /**
   * \brief This constructor creates a 3D vector and populates it
   * \param[in] el1
   * \param[in] el2
   * \param[in] el3
   * 
   * Create a new vector with el1, el2 and el3 as coordinates
   */
  svector(const double el1, const double el2, const double el3);

  /**
   * \brief Copy constructor.
   * \param v #svector
   * 
   * Create a new vector by copying the v vector.
   */
  svector(const svector& v);

  /**
   * \brief Get the 1st coordinate of the vector
   * \return The first element of the vector
   */
  double get_X() const;
 
  /**
   * \brief Get the 2nd coordinate of the vector.
   * \return the second element of the vector.
   */
  double get_Y() const; 
 
  /**
   * \brief Get the 3rd coordinate of the vector.
   * \return The third element of the vector.
   */
  double get_Z() const;

  /**
   * \brief Scalar product.
   * \param[in] u #svector
   * \return The scalar product of \f$\vec{this}\f$ by \f$\vec{u}\f$
   */
  double scalar(const svector& u) const;

  /**
   * \brief Vectorial product : \f$ \vec{Z} = \vec{this} \times \vec{Y}\f$.
   * \param[in] Y #svector
   * \param[out] Z #svector
   * 
   * This method computes the vectorial product with Y and 
   * it puts the result into Z.
   */
  void vectorialProduct(const svector& Y, svector& Z) const;

  /**
   * \brief Creation of an axis system with unit vectors.
   * \param[in] Y #svector
   * \param[out] M #smatrix
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
  void axisSystem(const svector Y, smatrix& M) const;

  /**
   * \brief Return the Euclidean norm of the vector.
   * \return The euclidean norm of the vector
   * 
   * This method computes the Euclidean norm of the vector \f$ \vec{this}=(m\_v1, m\_v2, m\_v3) \f$.
   * \f[
   * ||\vec{this}||_2 = \sqrt{m\_v1^2+m\_v2^2+m\_v3^2}
   * \f]
   */
  double norm2() const;

  /**
   * \brief Return the infinit norm of the vector.
   * \return The infinit norm of the vector
   * 
   * This method computes the infinit norm of the vector \f$ \vec{this}=(m\_v1, m\_v2, m\_v3) \f$.
   * \f[
   *   ||\vec{this}||_\infty = \max(|m\_v1|,|m\_v2|,|m\_v3|)
   * \f]
   */
  double norminf() const;

  /**
   * \brief Compute a colinear unit vector and store its length.
   * \param[in,out] _unitVector #svector
   * \param[out] length  The length of the vector \f$\vec{this}\f$
   * 
   * This method computes the colinear unit vector of \f$\vec{this}\f$
   * \f[
   *   \vec{unitvector} = \frac{\vec{this}}{||\vec{this}||_2}
   * \f]
   * and it computes the length of the vector \f$\vec{this}\f$
   * \f[
   *   length = ||\vec{this}||_2
   * \f]
   */
  void unitVector(svector &_unitVector, double &length) const;

  /**
   * \brief Multiplication by a matrix on its right and left.
   * this = this.M
   * \param[in] M #smatrix
   * 
   * This method computes the product of \f$\vec{this}\f$ by \f$M\f$
   * on its right
   * \f[\vec{this}=\vec{this}.M\f]
   */
  void multiplyOnTheRight(const smatrix &M);
 
  /**
   * \brief Multiplication by a matrix on its right and left.
   * this = M.this
   * \param[in] M #smatrix
   * 
   * This method computes the product of \f$\vec{this}\f$ by \f$M\f$
   * on its left 
   * \f[\vec{this} = M.\vec{this}\f]
   */
  void multiplyOnTheLeft(const smatrix &M);

  /**
   * \brief Printing on stdout the vector.
   * 
   * This method prints on stdout the coordinates of the vector \f$\vec{this}\f$ 
   */
  void printOnScreen() const;
};

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
  smatrix();

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
   * \brief Copy constructor.
   * \param M The original matrix
   *
   * Create a new matrix by copying the elements of the matrix \b M
   */
  smatrix(const smatrix& M);

  /**
   * \brief Copy a matrix.
   * \param M the original matrix
   *
   * Set the elements of the matrix \b this using the elements of the matrix \b M
   */
  void set(const smatrix& M);

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
  void transpose();

  /**
   * \brief Multiplication by the matrix \b M2 on its right.
   * \b this = \b this * \b M2
   * \param M2 a matrix
   *
   * Multiply the matrix \b this by \b M2 and put the results into \b this
   * \f[
   *    \mathbf{this} = \mathbf{this} \times \mathbf{M2}
   * \f]
   */
  void multiplyOnTheRight(const smatrix& M2);
  
  /**
   * \brief Multiplication by the matrix \b M2 on its left.
   * \b this = \b M2 * \b this
   * \param M2 a matrix
   *
   * Multiply the matrix \b M2 by \b this and put the results into \b this
   * \f[
   *    \mathbf{this} = \mathbf{M2} \times \mathbf{this}
   * \f]
   */
  void multiplyOnTheLeft(const smatrix& M2);

  /**
   * \brief Print on stdout the matrix \b this.
   */
  void printOnScreen() const;

  void testMultiplication(smatrix& M2);
};


#endif
