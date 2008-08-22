#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <string.h>

#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>
#include <hkl/hkl-quaternion.h>

void hkl_vector_init(HklVector *v, double x, double y, double z)
{
	v->data[0] = x;
	v->data[1] = y;
	v->data[2] = z;
}

void hkl_vector_fprintf(FILE *file, HklVector const *v)
{
	fprintf(file, "|%f, %f, %f|", v->data[0], v->data[1], v->data[2]);
}

int hkl_vector_cmp(HklVector const *v, HklVector const *v1)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(v->data[i] - v1->data[i]) > HKL_EPSILON )
			return HKL_TRUE;
	return HKL_FALSE;
}

/**not yet used*/
extern int hkl_vector_is_opposite(HklVector const *v, HklVector const *v1)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(v->data[i] + v1->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}

void hkl_vector_minus_vector(HklVector *v, HklVector const *v1)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] -= v1->data[i];
}

void hkl_vector_div_double(HklVector *v, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] /= d;
}

void hkl_vector_times_double(HklVector *v, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] *= d;
}

void hkl_vector_times_vector(HklVector *v, HklVector const *v1)
{
	unsigned int i;
	for (i=0;i<3;i++)
		v->data[i] *= v1->data[i];
}

void hkl_vector_times_smatrix(HklVector *v, HklMatrix const *m)
{
	HklVector tmp;
	tmp = *v;

	v->data[0] = tmp.data[0] *m->data[0][0] + tmp.data[1] *m->data[1][0] + tmp.data[2] *m->data[2][0];
	v->data[1] = tmp.data[0] *m->data[0][1] + tmp.data[1] *m->data[1][1] + tmp.data[2] *m->data[2][1];
	v->data[2] = tmp.data[0] *m->data[0][2] + tmp.data[1] *m->data[1][2] + tmp.data[2] *m->data[2][2];
}

double hkl_vector_sum(HklVector const *v)
{
	return v->data[0] + v->data[1] + v->data[2];
}

double hkl_vector_scalar_product(HklVector const *v, HklVector const *v1)
{
	unsigned int i;
	double scalar = 0;

	for (i=0;i<3;i++)
		scalar += v->data[i] *v1->data[i];
	return scalar;
}

void hkl_vector_vectorial_product(HklVector *v, HklVector const *v1)
{
	HklVector tmp;

	tmp = *v;
	v->data[0] = tmp.data[1] *v1->data[2] - tmp.data[2] *v1->data[1];
	v->data[1] = tmp.data[2] *v1->data[0] - tmp.data[0] *v1->data[2];
	v->data[2] = tmp.data[0] *v1->data[1] - tmp.data[1] *v1->data[0];
}


double hkl_vector_angle(HklVector const *v, HklVector const *v1)
{
	double angle;
	double cos_angle;
	double norm;
	double norm_v;
	double norm_v1;

	norm_v = hkl_vector_norm2(v);
	norm_v1 = hkl_vector_norm2(v1);

	// check the validity of the parameters
	assert(norm_v > HKL_EPSILON);
	assert(norm_v1 > HKL_EPSILON);

	norm = norm_v *norm_v1;

	cos_angle = hkl_vector_scalar_product(v, v1) / norm;

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

double hkl_vector_norm2(HklVector const *v)
{
	return sqrt(v->data[0] *v->data[0] + v->data[1] *v->data[1] + v->data[2] *v->data[2]);
}

/**
 *@brief normalize a hkl_vector
 *@return true if the hkl_vector can be normalized, false otherwise
 *@todo check the status
 */
int hkl_vector_normalize(HklVector *v)
{
	int status = HKL_FAIL;

	double norm = hkl_vector_norm2(v);
	if ( norm > HKL_EPSILON )
	{
		hkl_vector_div_double(v, norm);
		status = HKL_SUCCESS;
	}

	return status;
}

int hkl_vector_is_colinear(HklVector const *v, HklVector const *v1)
{
	int is_colinear = 0;
	HklVector tmp = *v;

	hkl_vector_vectorial_product(&tmp, v1);
	if (hkl_vector_norm2(&tmp) < HKL_EPSILON)
		is_colinear = 1;

	return is_colinear;
}


void hkl_vector_randomize(HklVector *v)
{
	v->data[0] = -1 + 2 *rand()/(RAND_MAX+1.0);
	v->data[1] = -1 + 2 *rand()/(RAND_MAX+1.0);
	v->data[2] = -1 + 2 *rand()/(RAND_MAX+1.0);
}

void hkl_vector_randomize_vector(HklVector *v, HklVector const *v1)
{
	do
		hkl_vector_randomize(v);
	while (!hkl_vector_cmp(v, v1));
}

void hkl_vector_randomize_vector_vector(HklVector *v, HklVector const *v1, HklVector const *v2)
{
	do
		hkl_vector_randomize(v);
	while (!hkl_vector_cmp(v, v1) || !hkl_vector_cmp(v, v2));
}

/**rotate a vector around another vector with an angle */
void hkl_vector_rotated_around_vector(HklVector *v, HklVector const *axe, double angle)
{
	double c = cos(angle);
	double s = sin(angle);
	HklVector axe_n;
	HklVector tmp;

	axe_n = *axe;
	hkl_vector_normalize(&axe_n);

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
 * apply a quaternion rotation to a vector 
 * @todo test
 */
void hkl_vector_rotated_quaternion(HklVector *v, HklQuaternion const *qr)
{
	HklQuaternion q;
	HklQuaternion tmp;

	// compute qr * qv * *qr
	q = *qr;
	hkl_quaternion_from_vector(&tmp, v);

	hkl_quaternion_times_quaternion(&q, &tmp);
	tmp = *qr;
	hkl_quaternion_conjugate(&tmp);
	hkl_quaternion_times_quaternion(&q, &tmp);

	// copy the vector part of the quaternion in the vector
	memcpy(v->data, &q.data[1], sizeof(v->data));
}

/**
 * @brief check if the hkl_vector is null
 * @return true if all |elements| are below HKL_EPSILON, false otherwise
 * @todo test
 */
int hkl_vector_is_null(HklVector const *v)
{
	unsigned int i;
	for (i=0; i<3; i++)
		if ( fabs(v->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}
