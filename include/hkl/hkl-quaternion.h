#ifndef __HKL_QUATERNION_H__
#define __HKL_QUATERNION_H__

#include <stdio.h>
#include <hkl/hkl-macros.h>
#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

struct _HklQuaternion
{
	double data[4];
};

extern void hkl_quaternion_fprintf(FILE *file, HklQuaternion const *q);

/*!
 *\brief This constructor creates a quaternion and populates it
 *\param v
 *
 *Create a new quaternion from a svector.
 */
extern void hkl_quaternion_from_svector(HklQuaternion *q, HklVector const *v);

/*!
 *\brief This constructor creates a quaternion from an angle and a vector
 *\param angle the rotation angle.
 *\param v the axe of the rotation.
 */
extern void hkl_quaternion_from_angle_and_axe(HklQuaternion *q, double angle, HklVector const *v);

extern int hkl_quaternion_cmp(HklQuaternion const *q, HklQuaternion const *q1);

extern void hkl_quaternion_minus_quaternion(HklQuaternion *q, HklQuaternion const *q1);

extern void hkl_quaternion_times_quaternion(HklQuaternion *q, HklQuaternion const *q1);

extern double hkl_quaternion_norm2(HklQuaternion const *q);

extern void hkl_quaternion_conjugate(HklQuaternion *q);

/*!
 *\brief Compute the rotation matrix of a Quaternion.
 *\return The rotation matrix of a Quaternion.
 *
 *to convert a quaternion to a Matrix:
 *\f$ q = a + b \cdot i + c \cdot j + d \cdot k \f$
 *
 *\f$
 *\left(
 *  \begin{array}{ccc}
 *    a^2+b^2-c^2-d^2 & 2bc-2ad         & 2ac+2bd\\
 *    2ad+2bc         & a^2-b^2+c^2-d^2 & 2cd-2ab\\
 *    2bd-2ac         & 2ab+2cd         & a^2-b^2-c^2+d^2
 *  \end{array}
 *\right)
 *\f$
 */
extern void hkl_quaternion_to_smatrix(HklQuaternion const *q, HklMatrix *m);

/*!
 *\brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
 *\param[out] angle The angle of the rotation will be strore in this variable.
 *\param[out] axe The axe of rotation will be store in this variable.
 */
extern void hkl_quaternion_to_angle_and_axe(HklQuaternion const *q, double *angle, HklVector *v);

HKL_END_DECLS

#endif
