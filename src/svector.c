#include "svecmat.h"

void hkl_svector_fprintf(FILE * file, struct hkl_svector const * v)
{
  fprintf(file, "|%f, %f, %f|", v->data[0], v->data[1], v->data[2]);
}

int hkl_svector_cmp(struct hkl_svector const * v, struct hkl_svector const * v1)
{
  unsigned int i;

  for (i=0; i<3; i++)
    if ( fabs(v->data[i] - v1->data[i]) > HKL_EPSILON )
      return HKL_FALSE;
  return HKL_TRUE;
}

void hkl_svector_minus_svector(struct hkl_svector * v, struct hkl_svector const * v1)
{
  unsigned int i;
  for (i=0;i<3;i++)
    v->data[i] -= v1->data[i];
}

void hkl_svector_div_double(struct hkl_svector * v, double const d)
{
  unsigned int i;
  for (i=0;i<3;i++)
    v->data[i] /= d;
}

void hkl_svector_times_double(struct hkl_svector * v, double const d)
{
  unsigned int i;
  for (i=0;i<3;i++)
    v->data[i] *= d;
}

void hkl_svector_times_svector(struct hkl_svector * v, struct hkl_svector const * v1)
{
  unsigned int i;
  for (i=0;i<3;i++)
    v->data[i] *= v1->data[i];
}

void hkl_svector_times_smatrix(struct hkl_svector * v, struct hkl_smatrix const * m)
{
  struct hkl_svector tmp;
  tmp = *v;

  v->data[0] = tmp.data[0] * m->data[0][0] + tmp.data[1] * m->data[1][0] + tmp.data[2] * m->data[2][0];
  v->data[1] = tmp.data[0] * m->data[0][1] + tmp.data[1] * m->data[1][1] + tmp.data[2] * m->data[2][1];
  v->data[2] = tmp.data[0] * m->data[0][2] + tmp.data[1] * m->data[1][2] + tmp.data[2] * m->data[2][2];
}

double hkl_svector_sum(struct hkl_svector const * v)
{
  return v->data[0] + v->data[1] + v->data[2];
}

double hkl_svector_scalar_product(struct hkl_svector const * v, struct hkl_svector const * v1)
{
  unsigned int i;
  double scalar = 0;

  for (i=0;i<3;i++)
    scalar += v->data[i] * v1->data[i];
  return scalar;
}

void hkl_svector_vectorial_product(struct hkl_svector * v, struct hkl_svector const * v1)
{
  struct hkl_svector tmp;

  tmp = *v;
  v->data[0] = tmp.data[1] * v1->data[2] - tmp.data[2] * v1->data[1];
  v->data[1] = tmp.data[2] * v1->data[0] - tmp.data[0] * v1->data[2];
  v->data[2] = tmp.data[0] * v1->data[1] - tmp.data[1] * v1->data[0];
}


double hkl_svector_angle(struct hkl_svector const * v, struct hkl_svector const * v1)
{
  double angle;
  double norm = hkl_svector_norm2(v) * hkl_svector_norm2(v1);

  double cosinus_angle = hkl_svector_scalar_product(v, v1) / norm;

  // problem with round
  if (cosinus_angle >= 1 )
    angle = 0;
  else
    if (cosinus_angle <= -1 )
      angle = M_PI;
    else
      angle = acos(cosinus_angle);

  return angle;
}

double hkl_svector_norm2(struct hkl_svector const * v)
{
  return sqrt(v->data[0] * v->data[0] + v->data[1] * v->data[1] + v->data[2] * v->data[2]);
}

/**
 * @brief normalize a hkl_svector
 * @return true if the hkl_svector can be normalized, false otherwise
 * @todo check the status
 */
int hkl_svector_normalize(struct hkl_svector * v)
{
  int status = HKL_FAIL;

  double norm = hkl_svector_norm2(v);
  if ( norm > HKL_EPSILON )
    {
      hkl_svector_div_double(v, norm);
      status = HKL_SUCCESS;
    }

  return status;
}

int hkl_svector_is_colinear(struct hkl_svector const * v, struct hkl_svector const * v1)
{
  int is_colinear = 0;
  struct hkl_svector tmp = *v;

  hkl_svector_vectorial_product(&tmp, v1);
  if (hkl_svector_norm2(&tmp) < HKL_EPSILON)
    is_colinear = 1;

  return is_colinear;
}


void hkl_svector_randomize(struct hkl_svector * v)
{
  v->data[0] = -1 + 2 * rand()/(RAND_MAX+1.0);
  v->data[1] = -1 + 2 * rand()/(RAND_MAX+1.0);
  v->data[2] = -1 + 2 * rand()/(RAND_MAX+1.0);
}

void hkl_svector_randomize_svector(struct hkl_svector * v, struct hkl_svector const * v1)
{
  do
    hkl_svector_randomize(v);
  while (hkl_svector_cmp(v, v1) == HKL_TRUE);
}

void hkl_svector_randomize_svector_svector(struct hkl_svector * v, struct hkl_svector const * v1, struct hkl_svector const * v2)
{
  do
    hkl_svector_randomize(v);
  while (hkl_svector_cmp(v, v1) == HKL_TRUE || hkl_svector_cmp(v, v2) == HKL_TRUE);
}


void hkl_svector_rotated_around_vector(struct hkl_svector * v, struct hkl_svector const * axe, double angle)
{
  double c = cos(angle);
  double s = sin(angle);
  struct hkl_svector axe_n;
  struct hkl_svector tmp;

  axe_n = *axe;
  hkl_svector_normalize(&axe_n);

  tmp = *v;

  v->data[0] = (c + (1 - c) * axe_n.data[0] * axe_n.data[0]) * tmp.data[0];
  v->data[0] += ((1 - c) * axe_n.data[0] * axe_n.data[1] - axe_n.data[2] * s) * tmp.data[1];
  v->data[0] += ((1 - c) * axe_n.data[0] * axe_n.data[2] + axe_n.data[1] * s) * tmp.data[2];

  v->data[1] = ((1 - c) * axe_n.data[0] * axe_n.data[1] + axe_n.data[2] * s) * tmp.data[0];
  v->data[1] += (c + (1 - c) * axe_n.data[1] * axe_n.data[1]) * tmp.data[1];
  v->data[1] += ((1 - c) * axe_n.data[1] * axe_n.data[2] - axe_n.data[0] * s) * tmp.data[2];

  v->data[2] = ((1 - c) * axe_n.data[0] * axe_n.data[2] - axe_n.data[1] * s) * tmp.data[0];
  v->data[2] += ((1 - c) * axe_n.data[1] * axe_n.data[2] + axe_n.data[0] * s) * tmp.data[1];
  v->data[2] += (c + (1 - c) * axe_n.data[2] * axe_n.data[2]) * tmp.data[2];
}

/**
 * @brief check if the hkl_svector is null
 * @return true if all |elements| are below HKL_EPSILON, false otherwise
 * @todo test
 */
int hkl_svector_is_null(struct hkl_svector const * v)
{
  unsigned int i;
  for (i=0; i<3; i++)
    if ( fabs(v->data[i]) > HKL_EPSILON )
      return HKL_FALSE;
  return HKL_TRUE;
}
