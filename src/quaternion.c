#include <string.h>
#include <assert.h>

#include "quaternion.h"


void hkl_quaternion_fprintf(FILE * file, struct hkl_quaternion const * q)
{
  fprintf(file, "<%f, %f, %f, %f>", q->data[0], q->data[1], q->data[2], q->data[3]);
}

void hkl_quaternion_from_svector(struct hkl_quaternion * q, struct hkl_svector const * v)
{
  q->data[0] = 0;
  memcpy(&q->data[1], &v->data[0], sizeof(v->data));
}


void hkl_quaternion_from_angle_and_axe(struct hkl_quaternion * q, double angle, struct hkl_svector const * v)
{
  double norm;
  double c;
  double s;

  // check that parameters are ok.
  assert(hkl_svector_norm2(v) > HKL_EPSILON);

  norm = hkl_svector_norm2(v);
  c = cos(angle / 2.);
  s = sin(angle / 2.);

  q->data[0] = c;
  q->data[1] = s * v->data[0] / norm;
  q->data[2] = s * v->data[1] / norm;
  q->data[3] = s * v->data[2] / norm;
}

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

double hkl_quaternion_norm2(struct hkl_quaternion const * q)
{
  double sum2 = 0;
  unsigned int i;
  for (i=0;i<4;i++)
    sum2 += q->data[i] * q->data[i];
  return sqrt(sum2);
}

void hkl_quaternion_conjugate(struct hkl_quaternion * q)
{
  unsigned int i;
  for (i=1;i<4;i++)
    q->data[i] = -q->data[i];
}


void hkl_quaternion_to_smatrix(struct hkl_quaternion const * q, struct hkl_smatrix * m)
{
  // check that parameters are ok.
  assert(hkl_quaternion_norm2(q) > HKL_EPSILON);

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

void hkl_quaternion_to_angle_and_axe(struct hkl_quaternion const * q, double * angle, struct hkl_svector * v)
{
  double norm;
  double cos_angle;
  double sin_angle;

  // check that parameters are ok.
  assert(hkl_quaternion_norm2(q) > HKL_EPSILON);

  norm = hkl_quaternion_norm2(q);
  // compute the angle
  cos_angle = q->data[0] / norm;
  *angle = 2 * acos(cos_angle);

  // compute the axe
  sin_angle = sin(*angle / 2);
  if (fabs(sin_angle) > HKL_EPSILON)
    {
      // compute the axe using the vector part of the quaterninon / norm
      memcpy(v->data, &q->data[1], sizeof(v->data));
      hkl_svector_div_double(v, sin_angle * norm);
    }
}
