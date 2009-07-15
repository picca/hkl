/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdlib.h>
#include <string.h>

#include <gsl/gsl_math.h>

#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>
#include <hkl/hkl-quaternion.h>

void hkl_vector_init(HklVector *v, double x, double y, double z)
{
	v->data[0] = x;
	v->data[1] = y;
	v->data[2] = z;
}

void hkl_vector_fprintf(FILE *file, HklVector const *self)
{
	fprintf(file, "|%f, %f, %f|", self->data[0], self->data[1], self->data[2]);
}

int hkl_vector_cmp(HklVector const *self, HklVector const *vector)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(self->data[i] - vector->data[i]) > HKL_EPSILON )
			return HKL_TRUE;
	return HKL_FALSE;
}

/**not yet used*/
int hkl_vector_is_opposite(HklVector const *self, HklVector const *vector)
{
	unsigned int i;

	for (i=0; i<3; i++)
		if ( fabs(self->data[i] + vector->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}

void hkl_vector_add_vector(HklVector *self, HklVector const *vector)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] += vector->data[i];
}

void hkl_vector_minus_vector(HklVector *self, HklVector const *vector)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] -= vector->data[i];
}

void hkl_vector_div_double(HklVector *self, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] /= d;
}

void hkl_vector_times_double(HklVector *self, double const d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] *= d;
}

void hkl_vector_times_vector(HklVector *self, HklVector const *vector)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] *= vector->data[i];
}

void hkl_vector_times_smatrix(HklVector *self, HklMatrix const *m)
{
	HklVector tmp;
	tmp = *self;

	self->data[0] = tmp.data[0] *m->data[0][0] + tmp.data[1] *m->data[1][0] + tmp.data[2] *m->data[2][0];
	self->data[1] = tmp.data[0] *m->data[0][1] + tmp.data[1] *m->data[1][1] + tmp.data[2] *m->data[2][1];
	self->data[2] = tmp.data[0] *m->data[0][2] + tmp.data[1] *m->data[1][2] + tmp.data[2] *m->data[2][2];
}

double hkl_vector_sum(HklVector const *self)
{
	return self->data[0] + self->data[1] + self->data[2];
}

double hkl_vector_scalar_product(HklVector const *self, HklVector const *vector)
{
	unsigned int i;
	double scalar = 0;

	for (i=0;i<3;i++)
		scalar += self->data[i] *vector->data[i];
	return scalar;
}

void hkl_vector_vectorial_product(HklVector *self, HklVector const *vector)
{
	HklVector tmp;

	tmp = *self;
	self->data[0] = tmp.data[1] * vector->data[2] - tmp.data[2] * vector->data[1];
	self->data[1] = tmp.data[2] * vector->data[0] - tmp.data[0] * vector->data[2];
	self->data[2] = tmp.data[0] * vector->data[1] - tmp.data[1] * vector->data[0];
}


double hkl_vector_angle(HklVector const *self, HklVector const *vector)
{
	double angle;
	double cos_angle;
	double norm;
	double norm_self;
	double norm_vector;

	norm_self = hkl_vector_norm2(self);
	norm_vector = hkl_vector_norm2(vector);

	if(norm_self < HKL_EPSILON || norm_vector < HKL_EPSILON)
		return GSL_NAN;

	norm = norm_self * norm_vector;

	cos_angle = hkl_vector_scalar_product(self, vector) / norm;

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

double hkl_vector_oriented_angle(HklVector const *self,
				 HklVector const *vector,
				 HklVector const *ref)
{
	double angle;
	HklVector tmp;
	HklVector ref_u;

	angle = hkl_vector_angle(self, vector);
	tmp = *self;
	hkl_vector_vectorial_product(&tmp, vector);
	hkl_vector_normalize(&tmp);
	ref_u = *ref;
	hkl_vector_normalize(&ref_u);
	if (hkl_vector_is_opposite(&tmp, &ref_u))
		angle = -angle;
	return angle;
}

/**
 *@brief normalize a hkl_vector
 *@return true if the hkl_vector can be normalized, false otherwise
 *@todo check the status
 */
int hkl_vector_normalize(HklVector *self)
{
	int status = HKL_FAIL;

	double norm = hkl_vector_norm2(self);
	if ( norm > HKL_EPSILON )
	{
		hkl_vector_div_double(self, norm);
		status = HKL_SUCCESS;
	}

	return status;
}

int hkl_vector_is_colinear(HklVector const *self, HklVector const *vector)
{
	int is_colinear = 0;
	HklVector tmp = *self;

	hkl_vector_vectorial_product(&tmp, vector);
	if (hkl_vector_norm2(&tmp) < HKL_EPSILON)
		is_colinear = 1;

	return is_colinear;
}


void hkl_vector_randomize(HklVector *self)
{
	self->data[0] = -1 + 2 *rand()/(RAND_MAX+1.0);
	self->data[1] = -1 + 2 *rand()/(RAND_MAX+1.0);
	self->data[2] = -1 + 2 *rand()/(RAND_MAX+1.0);
}

void hkl_vector_randomize_vector(HklVector *self, HklVector const *vector)
{
	do
		hkl_vector_randomize(self);
	while (!hkl_vector_cmp(self, vector));
}

void hkl_vector_randomize_vector_vector(HklVector *self,
					HklVector const *vector1,
					HklVector const *vector2)
{
	do
		hkl_vector_randomize(self);
	while (!hkl_vector_cmp(self, vector1) || !hkl_vector_cmp(self, vector2));
}

/**rotate a vector around another vector with an angle */
void hkl_vector_rotated_around_vector(HklVector *self,
				      HklVector const *axe, double angle)
{
	double c = cos(angle);
	double s = sin(angle);
	HklVector axe_n;
	HklVector tmp;

	axe_n = *axe;
	hkl_vector_normalize(&axe_n);

	tmp = *self;

	self->data[0]  = (c + (1 - c) * axe_n.data[0] * axe_n.data[0])                     * tmp.data[0];
	self->data[0] += ((1 - c)     * axe_n.data[0] * axe_n.data[1] - axe_n.data[2] * s) * tmp.data[1];
	self->data[0] += ((1 - c)     * axe_n.data[0] * axe_n.data[2] + axe_n.data[1] * s) * tmp.data[2];

	self->data[1]  = ((1 - c)     * axe_n.data[0] * axe_n.data[1] + axe_n.data[2] * s) * tmp.data[0];
	self->data[1] += (c + (1 - c) * axe_n.data[1] * axe_n.data[1])                     * tmp.data[1];
	self->data[1] += ((1 - c)     * axe_n.data[1] * axe_n.data[2] - axe_n.data[0] * s) * tmp.data[2];

	self->data[2]  = ((1 - c)     * axe_n.data[0] * axe_n.data[2] - axe_n.data[1] * s) * tmp.data[0];
	self->data[2] += ((1 - c)     * axe_n.data[1] * axe_n.data[2] + axe_n.data[0] * s) * tmp.data[1];
	self->data[2] += (c + (1 - c) * axe_n.data[2] * axe_n.data[2])                     * tmp.data[2];
}

double hkl_vector_norm2(HklVector const *self)
{
	return sqrt(self->data[0] * self->data[0]
		    + self->data[1] * self->data[1]
		    + self->data[2] * self->data[2]);
}

/**
 * apply a quaternion rotation to a vector 
 * @todo test
 */
void hkl_vector_rotated_quaternion(HklVector *self, HklQuaternion const *qr)
{
	HklQuaternion q;
	HklQuaternion tmp;

	// compute qr * qv * *qr
	q = *qr;
	hkl_quaternion_from_vector(&tmp, self);

	hkl_quaternion_times_quaternion(&q, &tmp);
	tmp = *qr;
	hkl_quaternion_conjugate(&tmp);
	hkl_quaternion_times_quaternion(&q, &tmp);

	// copy the vector part of the quaternion in the vector
	memcpy(self->data, &q.data[1], sizeof(self->data));
}

/**
 * @brief check if the hkl_vector is null
 * @return true if all |elements| are below HKL_EPSILON, false otherwise
 * @todo test
 */
int hkl_vector_is_null(HklVector const *self)
{
	unsigned int i;
	for (i=0; i<3; i++)
		if ( fabs(self->data[i]) > HKL_EPSILON )
			return HKL_FALSE;
	return HKL_TRUE;
}

void hkl_vector_project_on_plan(HklVector *self,
				HklVector const *plan)
{
	HklVector tmp;

	tmp = *plan;
	hkl_vector_normalize(&tmp);
	hkl_vector_times_double(&tmp, hkl_vector_scalar_product(self, &tmp));
	hkl_vector_minus_vector(self, &tmp);
}
