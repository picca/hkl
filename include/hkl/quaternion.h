#ifndef _QUATERNION_H
#define _QUATERNION_H

#include <stdlib.h>
#include <stdio.h>

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

    /* forward declaration */
    struct hkl_svector;
    struct hkl_smatrix;

    struct hkl_quaternion
      {
        double data[4];
      };

    extern void hkl_quaternion_fprintf(FILE * file, struct hkl_quaternion const * q);

    /*!
     * \brief This constructor creates a quaternion and populates it
     * \param v
     *
     * Create a new quaternion from a svector.
     */
    extern void hkl_quaternion_from_svector(struct hkl_quaternion * q, struct hkl_svector const * v);

    /*!
     * \brief This constructor creates a quaternion from an angle and a vector
     * \param angle the rotation angle.
     * \param v the axe of the rotation.
     */
    extern void hkl_quaternion_from_angle_and_axe(struct hkl_quaternion * q, double angle, struct hkl_svector const * v);

    extern int hkl_quaternion_cmp(struct hkl_quaternion const * q, struct hkl_quaternion const * q1);

    extern void hkl_quaternion_minus_quaternion(struct hkl_quaternion * q, struct hkl_quaternion const * q1);

    extern void hkl_quaternion_times_quaternion(struct hkl_quaternion * q, struct hkl_quaternion const * q1);

    extern double hkl_quaternion_norm2(struct hkl_quaternion const * q);

    extern void hkl_quaternion_conjugate(struct hkl_quaternion * q);

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
    extern void hkl_quaternion_to_smatrix(struct hkl_quaternion const * q, struct hkl_smatrix * m);

    /*!
     * \brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
     * \param[out] angle The angle of the rotation will be strore in this variable.
     * \param[out] axe The axe of rotation will be store in this variable.
     */
    extern void hkl_quaternion_to_angle_and_axe(struct hkl_quaternion const * q, double * angle, struct hkl_svector * v);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif
