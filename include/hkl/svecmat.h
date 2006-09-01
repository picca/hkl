#ifndef _SVECMAT_H_
#define _SVECMAT_H_

#include <math.h>
#include <iostream>
#include <iomanip>
#include <valarray>
#include <cstdlib>

#include "portability.h"
#include "HKLException.h"
#include "constants.h"

#define X 0
#define Y 1
#define Z 2

using namespace std;

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
     * \brief Randomize a vector with element coordinates randomly chosen between -1 and 1.
     */
    void randomize(void);

    /**
     * \brief Return a svector with element randomly chosen between -1 and 1 but not equal to v.
     * \param v The svector we do not want to be return.
     * \return The randomize svector.
     */
    svector & randomize(svector const & v);

    /**
     * \brief Return a svector with element randomly chosen between -1 and 1 but not equal to v1 and v2.
     * \param v1 The first svector we do not want to be return.
     * \param v2 The second svector we do not want to be return.
     * \return The randomize svector.
     */
    svector & randomize(svector const & v1, svector const & v2);

    /**
     * \brief rotate a vector around another one with an angle.
     * \param axe The svector corresponding to the rotation axe.
     * \param angle the angle of rotation.
     * \return The new vector.
     */
    svector rotatedAroundVector(svector const & axe, double const & angle) const;

    /**
     * \brief Save the svector into a stream.
     * \param flux the stream to save the svector into.
     * \return The stream with the svector.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * \brief Restore a svector from a stream.
     * \param flux The stream containing the svector.
     */
    istream & fromStream(istream & flux);
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
    
    /**
     * \brief Save the smatrix into a stream.
     * \param flux the stream to save the smatrix into.
     * \return The stream with the smatrix.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * \brief Restore a smatrix from a stream.
     * \param flux The stream containing the smatrix.
     */
    istream & fromStream(istream & flux);
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
