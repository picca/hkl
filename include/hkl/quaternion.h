#ifndef _QUATERNION_H
#define _QUATERNION_H


#include "svector.h"
#include <ostream>
#include <istream>

#include "convenience.h"
namespace hkl
  {

  /**
   * @todo Add the set unit Test
   */
  class Quaternion
    {
      friend class smatrix;
    protected:
      double _a;

      double _b;

      double _c;

      double _d;


    public:
      /*!
       * \brief Default constructor
       *
       * Create a new quaternion and set all its components to 0.0
       */
      Quaternion();

      /*!
       * \brief This constructor creates a quaternion and populates it
       * \param a
       * \param b
       * \param c
       * \param d
       *
       * Create a new quaternion with a, b, c and d as coordinates.
       */
      Quaternion(double a, double b, double c, double d);

      /*!
       * \brief This constructor creates a quaternion and populates it
       * \param v
       *
       * Create a new quaternion from a svector.
       */
      Quaternion(const hkl::svector & v);

      /*!
       * \brief This constructor creates a quaternion from an angle and a vector
       * \param angle the rotation angle.
       * \param v the axe of the rotation.
       */
      Quaternion(double angle, const hkl::svector & v);

      /*!
       * \brief Copy constructor.
       * \param q The Quaternion to copy from.
       */
      Quaternion(const Quaternion & source);

      /**
       * @brief Set the Quaternion parameters
       * @param a The 1st element.
       * @param b The 2nd element.
       * @param c The 3rd element.
       * @param d The 4th element.
       */
      void set(double a, double b, double c, double d);

      double & a();

      double & b();

      double & c();

      double & d();

      double const & a() const;

      double const & b() const;

      double const & c() const;

      double const & d() const;

      bool operator==(const Quaternion & q) const;

      /*!
       * \brief Add a Quaternion to another one.
       * \param q The Quaternion to add.
       * \return A reference to the Quaternion which was added.
       */
      Quaternion & operator+=(const Quaternion & q);

      /*!
       * \brief Substract a Quaternion to another one.
       * \param q The Quaternion to substract.
       * \return A reference to the Quaternion which was substracted.
       */
      Quaternion & operator-=(const Quaternion & q);

      /*!
       * \brief Multiply a Quaternion to another one.
       * \param q The Quaternion to multiply.
       * \return A reference to the Quaternion which was multiplyed.
       */
      Quaternion & operator*=(const Quaternion & q);

      /*!
       * \brief Divide a Quaternion by a double.
       * \param d The double to divide by.
       * \return A reference to the Quaternion which was modified.
       */
      Quaternion & operator/=(const double & d);

      /*!
       * \brief Compute de norm of the Quaternion.
       * \return The norme of the Quaternion.
       */
      double norm2() const;

      /*!
       * \brief Compute the conjugated Quaternion.
       * \return The conjugate Quaternion.
       */
      Quaternion conjugate() const;

      /*!
       * \brief Compute the dot product of a Quaternion.
       * \param q The Quaternion.
       * \return The dot Product.
       */
      double dotProduct(const Quaternion & q) const;

      /*!
       * \brief Compute the invert Quaternion.
       * \return The invert Quaternion.
       */
      Quaternion invert() const;

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
      hkl::smatrix asMatrix() const;

      /*!
       * \brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
       * \param[out] angle The angle of the rotation will be strore in this variable.
       * \param[out] axe The axe of rotation will be store in this variable.
       */
      void getAngleAndAxe(double & angle, hkl::svector & axe) const;

      /**
       * @brief Get the rotating axe of the Quaternion.
       * @return The rotating axe of the Quaternion.
       */
      hkl::svector getAxe() const;

      std::ostream & printToStream(std::ostream & flux) const;

      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a Quaternion from a stream.
       * \param flux The stream containing the Quaternion to restore.
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl

/**
 * \brief Surcharge de l'operateur << pour la class Quaternion
 * @param flux
 * @param m
 * @return
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Quaternion const & q)
{
  return q.printToStream(flux);
}
#endif
