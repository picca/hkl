#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <string.h>

#include <hkl/hklvector.h>
#include <hkl/hklmatrix.h>
#include <hkl/hklquaternion.h>

void hkl_svector_fprintf(FILE *file, HklVector const *v)
{
	fprintf(file, "|%f, %f, %f|", v->data[0], v->data[1], v->data[2]);
}

/**@todo test */
void hkl_svector_set(HklVector *v, double const x, double const y, double const z)
{
	v->data[0] = x;
	v->data[1] = y;
	v->data[2] = z;
}

int hkl_svector_cmp(HklVector const *v, HklVector const *v1)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(v->data[i] - v1->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}

/**not yet used*/
extern int hkl_svector_is_opposite(HklVector const *v, HklVector const *v1)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(v->data[i] + v1->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}

void hkl_svector_minus_svector(HklVector *v, HklVector const *v1)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] -= v1->data[i];
}

void hkl_svector_div_double(HklVector *v, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] /= d;
}

void hkl_svector_times_double(HklVector *v, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] *= d;
}

void hkl_svector_times_svector(HklVector *v, HklVector const *v1)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] *= v1->data[i];
}

void hkl_svector_times_smatrix(HklVector *v, HklMatrix const *m)
{
	HklVector tmp;
	tmp = *v;

	v->data[0] = tmp.data[0] *m->data[0][0] + tmp.data[1] *m->data[1][0] + tmp.data[2] *m->data[2][0];
	v->data[1] = tmp.data[0] *m->data[0][1] + tmp.data[1] *m->data[1][1] + tmp.data[2] *m->data[2][1];
	v->data[2] = tmp.data[0] *m->data[0][2] + tmp.data[1] *m->data[1][2] + tmp.data[2] *m->data[2][2];
}

double hkl_svector_sum(HklVector const *v)
{
	return v->data[0] + v->data[1] + v->data[2];
}

double hkl_svector_scalar_product(HklVector const *v, HklVector const *v1)
{
	unsigned int i;
	double scalar = 0;

	for (i=0;i<3;i++)
		scalar += v->data[i] *v1->data[i];
	return scalar;
}

void hkl_svector_vectorial_product(HklVector *v, HklVector const *v1)
{
	HklVector tmp;

	tmp = *v;
	v->data[0] = tmp.data[1] *v1->data[2] - tmp.data[2] *v1->data[1];
	v->data[1] = tmp.data[2] *v1->data[0] - tmp.data[0] *v1->data[2];
	v->data[2] = tmp.data[0] *v1->data[1] - tmp.data[1] *v1->data[0];
}


double hkl_svector_angle(HklVector const *v, HklVector const *v1)
{
	double angle;
	double cos_angle;
	double norm;
	double norm_v;
	double norm_v1;

	norm_v = hkl_svector_norm2(v);
	norm_v1 = hkl_svector_norm2(v1);

	// check the validity of the parameters
	assert(norm_v > HKL_EPSILON);
	assert(norm_v1 > HKL_EPSILON);

	norm = norm_v *norm_v1;

	cos_angle = hkl_svector_scalar_product(v, v1) / norm;

	// problem with round
	if (cos_angle >= 1 )
		angle = 0;
	else
		if (cos_angle <= -1 )
			angle = M_PI;
		else
			angle = acos(cos_angle);
	return angle;
}

double hkl_svector_norm2(HklVector const *v)
{
	return sqrt(v->data[0] *v->data[0] + v->data[1] *v->data[1] + v->data[2] *v->data[2]);
}

/**
 *@brief normalize a hkl_svector
 *@return true if the hkl_svector can be normalized, false otherwise
 *@todo check the status
 */
int hkl_svector_normalize(HklVector *v)
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

int hkl_svector_is_colinear(HklVector const *v, HklVector const *v1)
{
	int is_colinear = 0;
	HklVector tmp = *v;

	hkl_svector_vectorial_product(&tmp, v1);
	if (hkl_svector_norm2(&tmp) < HKL_EPSILON)
		is_colinear = 1;

	return is_colinear;
}


void hkl_svector_randomize(HklVector *v)
{
	v->data[0] = -1 + 2 *rand()/(RAND_MAX+1.0);
	v->data[1] = -1 + 2 *rand()/(RAND_MAX+1.0);
	v->data[2] = -1 + 2 *rand()/(RAND_MAX+1.0);
}

void hkl_svector_randomize_svector(HklVector *v, HklVector const *v1)
{
	do
		hkl_svector_randomize(v);
	while (hkl_svector_cmp(v, v1) == HKL_TRUE);
}

void hkl_svector_randomize_svector_svector(HklVector *v, HklVector const *v1, HklVector const *v2)
{
	do
		hkl_svector_randomize(v);
	while (hkl_svector_cmp(v, v1) == HKL_TRUE || hkl_svector_cmp(v, v2) == HKL_TRUE);
}

/**rotate a svector around another svector with an angle */
void hkl_svector_rotated_around_vector(HklVector *v, HklVector const *axe, double angle)
{
	double c = cos(angle);
	double s = sin(angle);
	HklVector axe_n;
	HklVector tmp;

	axe_n = *axe;
	hkl_svector_normalize(&axe_n);

	tmp = *v;

	v->data[0] = (c + (1 - c) *axe_n.data[0] *axe_n.data[0]) *tmp.data[0];
	v->data[0] += ((1 - c) *axe_n.data[0] *axe_n.data[1] - axe_n.data[2] *s) *tmp.data[1];
	v->data[0] += ((1 - c) *axe_n.data[0] *axe_n.data[2] + axe_n.data[1] *s) *tmp.data[2];

	v->data[1] = ((1 - c) *axe_n.data[0] *axe_n.data[1] + axe_n.data[2] *s) *tmp.data[0];
	v->data[1] += (c + (1 - c) *axe_n.data[1] *axe_n.data[1]) *tmp.data[1];
	v->data[1] += ((1 - c) *axe_n.data[1] *axe_n.data[2] - axe_n.data[0] *s) *tmp.data[2];

	v->data[2] = ((1 - c) *axe_n.data[0] *axe_n.data[2] - axe_n.data[1] *s) *tmp.data[0];
	v->data[2] += ((1 - c) *axe_n.data[1] *axe_n.data[2] + axe_n.data[0] *s) *tmp.data[1];
	v->data[2] += (c + (1 - c) *axe_n.data[2] *axe_n.data[2]) *tmp.data[2];
}

/**
 * apply a quaternion rotation to a svector 
 * @todo test
 */
void hkl_svector_rotated_quaternion(HklVector *v, HklQuaternion const *qr)
{
	HklQuaternion q;
	HklQuaternion tmp;

	// compute qr * qv * *qr
	q = *qr;
	hkl_quaternion_from_svector(&tmp, v);

	hkl_quaternion_times_quaternion(&q, &tmp);
	tmp = *qr;
	hkl_quaternion_conjugate(&tmp);
	hkl_quaternion_times_quaternion(&q, &tmp);

	// copy the vector part of the quaternion in the vector
	memcpy(v->data, &q.data[1], sizeof(v->data));
}

/**
 * @brief check if the hkl_svector is null
 * @return true if all |elements| are below HKL_EPSILON, false otherwise
 * @todo test
 */
int hkl_svector_is_null(HklVector const *v)
{
	unsigned int i;
	for (i=0; i<3; i++)
		if ( fabs(v->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}
