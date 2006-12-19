#ifndef _QUATERNION_H_
#define _QUATERNION_H_

#include <iostream>
#include <valarray>
#include "HKLException.h"
#include "svecmat.h"
#include "constants.h"

namespace hkl
  {

  /*!
   * \brief Define a quaternion in a four dimensionnal space.
   */
  class Quaternion : public valarray<double>
    {
    public:
      /*!
       * \brief Default constructor
       * 
       * Create a new quaternion and set all its components to 0.0
       */
      Quaternion(void);

      /*!
       * \brief This constructor creates a quaternion and populates it
       * \param el1
       * \param el2
       * \param el3
       * \param el4
       * 
       * Create a new quaternion with el1, el2, el3 and el4 as coordinates.
       */
      Quaternion(double const & el1, double const & el2, double const & el3, double const & el4);

      /*!
       * \brief This constructor creates a quaternion from an angle and a vector
       * \param angle the rotation angle.
       * \param v the axe of the rotation.
       */
      Quaternion(double const & angle, svector const & v);

      /**
       * @brief Tis constructor creates a quaternion from a svector
       * @param v The vectorial part of the quaternion.
       */
      Quaternion(svector const & v);

      /*!
       * \brief Copy constructor.
       * \param q The Quaternion to copy from.
       */
      Quaternion(Quaternion const & q);

      /*!
       * \brief Compare two quaternions
       * \param q the Quaternion tocompare with. 
       * \return 1 if this == q, 0 otherwise
       */
      bool operator == (Quaternion const & q) const;

      /*!
       * \brief Multiply a Quaternion to another one.
       * \param q The Quaternion to multiply.
       * \return A reference to the Quaternion which was multiplyed.
       */
      Quaternion & operator *=(Quaternion const & q);

      /*!
       * \brief Divide a Quaternion by a double.
       * \param d The double to divide by.
       * \return A reference to the Quaternion which was modified.
       */
      Quaternion & operator /=(double const & d);

      /*!
       * \brief Compute de norm of the Quaternion.
       * \return The norme of the Quaternion.
       */
      double norm2(void) const;

      /*!
       * \brief Compute the conjugated Quaternion.
       * \return The conjugate Quaternion.
       */
      Quaternion conjugate(void) const;

      /*!
       * \brief Compute the dot product of a Quaternion.
       * \param q The Quaternion.
       * \return The dot Product.
       */
      double dotProduct(Quaternion const & q) const;

      /*!
       * \brief Compute the invert Quaternion.
       * \return The invert Quaternion.
       */
      Quaternion invert(void) const;

      /*!
       * \brief Compute the rotation matrix of a Quaternion.
       * \return The rotation matrix of a Quaternion.
       *
       * to convert a quaternion to a Matrix:
       * \f$ q = a + b \cdot i + c \cdot j + d \cdot k \f$
       * 
       * \f$
       * \left(
       *   \begin{array}{ccc}
       *     a^2+b^2-c^2-d^2 & 2bc-2ad         & 2ac+2bd\\
       *     2ad+2bc         & a^2-b^2+c^2-d^2 & 2cd-2ab\\
       *     2bd-2ac         & 2ab+2cd         & a^2-b^2-c^2+d^2
       *   \end{array}
       * \right)
       * \f$
       */
      smatrix asMatrix(void) const;

      /*!
       * \brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
       * \param[out] angle The angle of the rotation will be strore in this variable.
       * \param[out] axe The axe of rotation will be store in this variable.
       */
      void getAngleAndAxe(double & angle, svector & axe) const;

      /**
       * @brief Get the rotating axe of the Quaternion.
       * @return The rotating axe of the Quaternion.
       */
      svector getAxe(void) const;

      /*!
       * \brief Save the Quaternion into a stream.
       * \param flux the stream to save the Quaternion into.
       * \return The stream with the Quaternion.
       */
      ostream & toStream(ostream & flux) const;

      /*!
       * \brief Restore a Quaternion from a stream.
       * \param flux The stream containing the Quaternion.
       * \return The modified stream.
       */
      istream & fromStream(istream & flux);

    private:
      valarray<double> m_data; //!< The valarray containing all the quaternion coordinates.
    };

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class quaternion
 * \param flux 
 * \param q 
 * \return 
 */
ostream & operator << (ostream & flux, hkl::Quaternion const & q);

#endif
