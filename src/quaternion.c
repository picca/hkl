#include <math.h>
#include <string.h>
#include <assert.h>

#include "config.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

/** print into a file a quaternion */
void hkl_quaternion_fprintf(FILE * file, struct hkl_quaternion const * q)
{
  fprintf(file, "<%f, %f, %f, %f>", q->data[0], q->data[1], q->data[2], q->data[3]);
}

/** create an hkl_quaternion from an hkl_svector */
void hkl_quaternion_from_svector(struct hkl_quaternion * q, struct hkl_svector const * v)
{
  q->data[0] = 0;
  memcpy(&q->data[1], &v->data[0], sizeof(v->data));
}

/** create an hkl_quaternion from an angle and an axe */
void hkl_quaternion_from_angle_and_axe(struct hkl_quaternion * q, double angle, struct hkl_svector const * v)
{
  double norm;
  double c;
  double s;

  // check that parameters are ok.
  norm = hkl_svector_norm2(v);
  assert(norm > HKL_EPSILON);

  c = cos(angle / 2.);
  s = sin(angle / 2.);

  q->data[0] = c;
  q->data[1] = s * v->data[0] / norm;
  q->data[2] = s * v->data[1] / norm;
  q->data[3] = s * v->data[2] / norm;
}

/** compare two hkl_quaternions */
int hkl_quaternion_cmp(struct hkl_quaternion const * q, struct hkl_quaternion const * q1)
{
  unsigned int i;

  for (i=0;i<4;i++)
    if ( fabs(q->data[i] - q1->data[i]) > HKL_EPSILON )
      return HKL_FALSE;
  return HKL_TRUE;
}

/** @todo test */
void hkl_quaternion_minus_quaternion(struct hkl_quaternion * q, struct hkl_quaternion const * q1)
{
  unsigned int i;

  for (i=0;i<4;i++)
    q->data[i] -= q1->data[i];
}

/** multiplication of two quaternions */
void hkl_quaternion_times_quaternion(struct hkl_quaternion * q, struct hkl_quaternion const * q1)
{
  struct hkl_quaternion Q;
  struct hkl_quaternion const * Q1;

  Q = *q;
  if (q == q1)
    Q1 = &Q;
  else
    Q1 = q1;

  q->data[0] = Q.data[0]*Q1->data[0] - Q.data[1]*Q1->data[1] - Q.data[2]*Q1->data[2] - Q.data[3]*Q1->data[3];
  q->data[1] = Q.data[0]*Q1->data[1] + Q.data[1]*Q1->data[0] + Q.data[2]*Q1->data[3] - Q.data[3]*Q1->data[2];
  q->data[2] = Q.data[0]*Q1->data[2] - Q.data[1]*Q1->data[3] + Q.data[2]*Q1->data[0] + Q.data[3]*Q1->data[1];
  q->data[3] = Q.data[0]*Q1->data[3] + Q.data[1]*Q1->data[2] - Q.data[2]*Q1->data[1] + Q.data[3]*Q1->data[0];
}

/** compute the norm of a quaternion */
double hkl_quaternion_norm2(struct hkl_quaternion const * q)
{
  double sum2 = 0;
  unsigned int i;
  for (i=0;i<4;i++)
    sum2 += q->data[i] * q->data[i];
  return sqrt(sum2);
}

/** compute the conjugate of a quaternion */
void hkl_quaternion_conjugate(struct hkl_quaternion * q)
{
  unsigned int i;
  for (i=1;i<4;i++)
    q->data[i] = -q->data[i];
}

/**
 * @brief Compute the rotation matrix of a Quaternion.
 * \return The rotation matrix of a Quaternion.
 *
 * compute the rotation matrix corresponding to the unitary quaternion.
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
void hkl_quaternion_to_smatrix(struct hkl_quaternion const * q, struct hkl_smatrix * m)
{
  // check that parameters are ok.
  assert(fabs(hkl_quaternion_norm2(q) - 1) < HKL_EPSILON);

  m->data[0][0] = q->data[0] * q->data[0] + q->data[1] * q->data[1] - q->data[2] * q->data[2] - q->data[3] * q->data[3];
  m->data[0][1] = 2 * (q->data[1] * q->data[2] - q->data[0] * q->data[3]);
  m->data[0][2] = 2 * (q->data[0] * q->data[2] + q->data[1] * q->data[3]);

  m->data[1][0] = 2 * (q->data[0] * q->data[3] + q->data[1] * q->data[2]);
  m->data[1][1] = q->data[0] * q->data[0] - q->data[1] * q->data[1] + q->data[2] * q->data[2] - q->data[3] * q->data[3];
  m->data[1][2] = 2 * (q->data[2] * q->data[3] - q->data[0] * q->data[1]);

  m->data[2][0] = 2 * (q->data[1] * q->data[3] - q->data[0] * q->data[2]);
  m->data[2][1] = 2 * (q->data[0] * q->data[1] + q->data[2] * q->data[3]);
  m->data[2][2] = q->data[0] * q->data[0] - q->data[1] * q->data[1] - q->data[2] * q->data[2] + q->data[3] * q->data[3];
}

/** 
 * compute the axe and angle of the unitary quaternion angle [-pi, pi]
 * if q is the (1, 0, 0, 0) quaternion return the (0,0,0) axe and a 0 angle
 */
void hkl_quaternion_to_angle_and_axe(struct hkl_quaternion const * q, double * angle, struct hkl_svector * v)
{
  double angle_2;
  double cos_angle_2;
  double sin_angle_2;

  // check that parameters are ok. (norm must be equal to 1)
  assert(fabs(hkl_quaternion_norm2(q) - 1) < HKL_EPSILON);

  // compute the angle
  cos_angle_2 = q->data[0];
  angle_2 = acos(cos_angle_2);
  *angle = 2 * angle_2;
  // we want an angle between -pi, pi
  if (*angle > M_PI) *angle -= 2 * M_PI;

  // compute the axe
  sin_angle_2 = sin(angle_2);
  if (fabs(sin_angle_2) > HKL_EPSILON)
    {
      // compute the axe using the vector part of the unitary quaterninon
      memcpy(v->data, &q->data[1], sizeof(v->data));
      hkl_svector_div_double(v, sin_angle_2);
    }
  else
    {
      *angle = 0;
      memset(v->data, 0, sizeof(v->data));
    }
}
