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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

/* for strdup */
#define _XOPEN_SOURCE 500
#include <string.h>

#include <gsl/gsl_multimin.h>

#include <hkl/hkl-sample.h>
#include <hkl/hkl-matrix.h>

#include "hkl-axis-private.h"
#include "hkl-detector-private.h"
#include "hkl-parameter-private.h"
#include "hkl-geometry-private.h"

/* private */

static void hkl_sample_reflection_update(HklSampleReflection *self)
{
	HklVector ki;
	HklQuaternion q;

	if(!self)
		return;

	/* compute the _hkl using only the axes of the geometry */
	/* first Q from angles */
	hkl_source_compute_ki(&self->geometry->source, &ki);
	self->_hkl = ki;
	hkl_vector_rotated_quaternion(&self->_hkl,
				      &darray_item(self->geometry->holders, self->detector->idx)->q);
	hkl_vector_minus_vector(&self->_hkl, &ki);

	q = darray_item(self->geometry->holders, 0)->q;
	hkl_quaternion_conjugate(&q);
	hkl_vector_rotated_quaternion(&self->_hkl, &q);
}

static void hkl_sample_compute_UxUyUz(HklSample *self)
{
	double ux;
	double uy;
	double uz;

	hkl_matrix_to_euler(&self->U, &ux, &uy, &uz);
	hkl_parameter_set_value(self->ux, ux, NULL);
	hkl_parameter_set_value(self->uy, uy, NULL);
	hkl_parameter_set_value(self->uz, uz, NULL);
}

static int hkl_sample_compute_UB(HklSample *self)
{
	HklMatrix B;

	if (!hkl_lattice_get_B(self->lattice, &B))
		return HKL_FALSE;

	self->UB = self->U;
	hkl_matrix_times_matrix(&self->UB, &B);

	return HKL_TRUE;
}

/*
 * this structure is used by the minimization gsl algorithm.
 * in the set_UB method
 */
struct set_UB_t
{
	HklSample *sample;
	const HklMatrix *UB;
};

static double set_UB_fitness(const gsl_vector *x, void *params)
{
	size_t i, j;
	double fitness;
	double euler_x;
	double euler_y;
	double euler_z;
	struct set_UB_t *parameters = params;
	HklSample *sample = parameters->sample;
	const HklMatrix *UB = parameters->UB;

	euler_x = gsl_vector_get(x, 0);
	euler_y = gsl_vector_get(x, 1);
	euler_z = gsl_vector_get(x, 2);

	hkl_parameter_set_value(sample->ux, euler_x, NULL);
	hkl_parameter_set_value(sample->uy, euler_y, NULL);
	hkl_parameter_set_value(sample->uz, euler_z, NULL);
	hkl_parameter_set_value(sample->lattice->a, gsl_vector_get(x, 3), NULL);
	hkl_parameter_set_value(sample->lattice->b, gsl_vector_get(x, 4), NULL);
	hkl_parameter_set_value(sample->lattice->c, gsl_vector_get(x, 5), NULL);
	hkl_parameter_set_value(sample->lattice->alpha, gsl_vector_get(x, 6), NULL);
	hkl_parameter_set_value(sample->lattice->beta, gsl_vector_get(x, 7), NULL);
	hkl_parameter_set_value(sample->lattice->gamma, gsl_vector_get(x, 8), NULL);

	hkl_matrix_init_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (!hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<3; ++i)
		for(j=0; j<3; ++j){
			double tmp = UB->data[i][j] - sample->UB.data[i][j];
			fitness += tmp * tmp;
		}
	return fitness;
}

static double mono_crystal_fitness(const gsl_vector *x, void *params)
{
	size_t i, j;
	double fitness;
	double euler_x;
	double euler_y;
	double euler_z;
	HklSample *sample = params;

	euler_x = gsl_vector_get(x, 0);
	euler_y = gsl_vector_get(x, 1);
	euler_z = gsl_vector_get(x, 2);

	hkl_parameter_set_value(sample->ux, euler_x, NULL);
	hkl_parameter_set_value(sample->uy, euler_y, NULL);
	hkl_parameter_set_value(sample->uz, euler_z, NULL);
	hkl_parameter_set_value(sample->lattice->a, gsl_vector_get(x, 3), NULL);
	hkl_parameter_set_value(sample->lattice->b, gsl_vector_get(x, 4), NULL);
	hkl_parameter_set_value(sample->lattice->c, gsl_vector_get(x, 5), NULL);
	hkl_parameter_set_value(sample->lattice->alpha, gsl_vector_get(x, 6), NULL);
	hkl_parameter_set_value(sample->lattice->beta, gsl_vector_get(x, 7), NULL);
	hkl_parameter_set_value(sample->lattice->gamma, gsl_vector_get(x, 8), NULL);
	hkl_matrix_init_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (!hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<sample->reflections_len; ++i) {
		HklSampleReflection *reflection;

		reflection = sample->reflections[i];
		if(reflection->flag){
			HklVector UBh;

			UBh = reflection->hkl;
			hkl_matrix_times_vector(&sample->UB, &UBh);

			for(j=0; j<3; ++j) {
				double tmp = UBh.data[j] - reflection->_hkl.data[j];
				fitness += tmp * tmp;
			}
		}
	}
	return fitness;
}

static double minimize(HklSample *sample, double (* f) (const gsl_vector * x, void * params), void *params)
{
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size = 0;

	if (!sample)
		return GSL_NAN;

	/* Starting point */
	x = gsl_vector_alloc (9);
	gsl_vector_set (x, 0, hkl_parameter_get_value(sample->ux));
	gsl_vector_set (x, 1, hkl_parameter_get_value(sample->uy));
	gsl_vector_set (x, 2, hkl_parameter_get_value(sample->uz));
	gsl_vector_set (x, 3, hkl_parameter_get_value(sample->lattice->a));
	gsl_vector_set (x, 4, hkl_parameter_get_value(sample->lattice->b));
	gsl_vector_set (x, 5, hkl_parameter_get_value(sample->lattice->c));
	gsl_vector_set (x, 6, hkl_parameter_get_value(sample->lattice->alpha));
	gsl_vector_set (x, 7, hkl_parameter_get_value(sample->lattice->beta));
	gsl_vector_set (x, 8, hkl_parameter_get_value(sample->lattice->gamma));

	/* Set initial step sizes to 1 */
	ss = gsl_vector_alloc (9);
	gsl_vector_set (ss, 0, sample->ux->fit);
	gsl_vector_set (ss, 1, sample->uy->fit);
	gsl_vector_set (ss, 2, sample->uz->fit);
	gsl_vector_set (ss, 3, sample->lattice->a->fit);
	gsl_vector_set (ss, 4, sample->lattice->b->fit);
	gsl_vector_set (ss, 5, sample->lattice->c->fit);
	gsl_vector_set (ss, 6, sample->lattice->alpha->fit);
	gsl_vector_set (ss, 7, sample->lattice->beta->fit);
	gsl_vector_set (ss, 8, sample->lattice->gamma->fit);

	/* Initialize method and iterate */
	minex_func.n = 9;
	minex_func.f = f;
	minex_func.params = params;
	s = gsl_multimin_fminimizer_alloc (T, 9);
	gsl_set_error_handler_off();
	gsl_multimin_fminimizer_set (s, &minex_func, x, ss);
	do {
		++iter;
		status = gsl_multimin_fminimizer_iterate(s);
		if (status)
			break;
		size = gsl_multimin_fminimizer_size (s);
		status = gsl_multimin_test_size (size, HKL_EPSILON / 2.);
	} while (status == GSL_CONTINUE && iter < 10000);
	gsl_vector_free(x);
	gsl_vector_free(ss);
	gsl_multimin_fminimizer_free(s);
	gsl_set_error_handler (NULL);

	return size;
}

/*************/
/* HklSample */
/*************/

/**
 * hkl_sample_new:
 * @name:
 * @type:
 *
 * constructor
 *
 * Returns:
 **/
HklSample* hkl_sample_new(const char *name, HklSampleType type)
{
	HklSample *self = NULL;

	/* check parameters */
	if(!name)
		return self;

	self = HKL_MALLOC(HklSample);

	self->name = strdup(name);
	self->type = type;
	self->lattice = hkl_lattice_new_default();
	hkl_matrix_init(&self->U,1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_matrix_init(&self->UB,1, 0, 0, 0, 1, 0, 0, 0, 1);

	self->ux = hkl_parameter_new("ux", -M_PI, 0., M_PI,
				     HKL_TRUE, HKL_TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);
	self->uy = hkl_parameter_new("uy", -M_PI, 0., M_PI,
				     HKL_TRUE, HKL_TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);
	self->uz = hkl_parameter_new("uz", -M_PI, 0., M_PI,
				     HKL_TRUE, HKL_TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);

	hkl_sample_compute_UB(self);
	self->reflections = NULL;
	self->reflections_len = 0;

	return self;
}

/**
 * hkl_sample_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklSample *hkl_sample_new_copy(const HklSample *self)
{
	HklSample *dup = NULL;
	size_t len;
	size_t i;

	/* check parameters */
	if(!self)
		return dup;

	dup = HKL_MALLOC(HklSample);

	dup->name = strdup(self->name);
	dup->type = self->type;
	dup->lattice = hkl_lattice_new_copy(self->lattice);
	dup->U = self->U;
	dup->UB = self->UB;
	dup->ux = hkl_parameter_new_copy(self->ux);
	dup->uy = hkl_parameter_new_copy(self->uy);
	dup->uz = hkl_parameter_new_copy(self->uz);

	/* copy the reflections */
	dup->reflections = malloc(sizeof(*dup->reflections) * self->reflections_len);
	dup->reflections_len = self->reflections_len;
	for(i=0; i<dup->reflections_len; ++i)
		self->reflections[i] = hkl_sample_reflection_new_copy(self->reflections[i]);

	return dup;
}

/**
 * hkl_sample_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_sample_free(HklSample *self)
{
	size_t i;

	if (!self)
		return;

	free(self->name);
	hkl_lattice_free(self->lattice);
	hkl_parameter_free(self->ux);
	hkl_parameter_free(self->uy);
	hkl_parameter_free(self->uz);
	for(i=0; i<self->reflections_len;  ++i)
		hkl_sample_reflection_free(self->reflections[i]);
	if(self->reflections){
		free(self->reflections);
		self->reflections = NULL;
		self->reflections_len = 0;
	}
	free(self);
}

/**
 * hkl_sample_set_name: (skip)
 * @self:
 * @name:
 *
 * set the name of the sample
 **/
void hkl_sample_set_name(HklSample *self, const char *name)
{
	if (!self)
		return;

	if(self->name)
		free(self->name);
	self->name = strdup(name);
}

/**
 * hkl_sample_set_lattice:
 * @self:
 * @a:
 * @b:
 * @c:
 * @alpha:
 * @beta:
 * @gamma:
 *
 * set the lattic eparameters of the sample
 *
 * Returns:
 **/
int hkl_sample_set_lattice(HklSample *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma)
{
	if (!self || !hkl_lattice_set(self->lattice, a, b, c, alpha, beta, gamma))
		return HKL_FALSE;

	hkl_sample_compute_UB(self);

	return HKL_TRUE;
}

/**
 * hkl_sample_set_U_from_euler: (skip)
 * @self:
 * @x:
 * @y:
 * @z:
 *
 * set the U matrix using the eulerians angles
 * todo tests
 *
 * Returns:
 **/
int hkl_sample_set_U_from_euler(HklSample *self,
				double x, double y, double z)
{
	if (!self)
		return HKL_FALSE;

	hkl_matrix_init_from_euler(&self->U, x, y, z);
	hkl_sample_compute_UB(self);
	hkl_parameter_set_value(self->ux, x, NULL);
	hkl_parameter_set_value(self->uy, y, NULL);
	hkl_parameter_set_value(self->uz, z, NULL);

	return HKL_TRUE;
}

/**
 * hkl_sample_get_UB: (skip)
 * @self:
 * @UB: (inout): where to store the UB matrix
 *
 * get the UB matrix of the sample
 **/
void hkl_sample_get_UB(HklSample *self, HklMatrix *UB)
{
	if (!self || !UB)
		return;

	hkl_sample_compute_UB(self);
	*UB = self->UB;
}

/**
 * hkl_sample_set_UB: (skip)
 * @self: the sample to modify
 * @UB: (in): the UB matrix to set
 *
 * Set the UB matrix using an external UB matrix. In fact you give
 * the UB matrix but only the U matrix of the sample is affected by
 * this operation. We keep the B matrix constant.
 * U * B = UB -> U = UB * B^-1
 **/
double hkl_sample_set_UB(HklSample *self, const HklMatrix *UB)
{
	struct set_UB_t params;

	if(!self || !UB)
		return -1;

	params.sample = self;
	params.UB = UB;

	return minimize(self, set_UB_fitness, &params);
}

/**
 * hkl_sample_add_reflection: (skip)
 * @self:
 * @geometry:
 * @detector:
 * @h:
 * @k:
 * @l:
 *
 * add a reflection to the sample
 *
 * Returns:
 **/
HklSampleReflection *hkl_sample_add_reflection(HklSample *self,
					       HklGeometry *geometry,
					       const HklDetector *detector,
					       double h, double k, double l)
{
	HklSampleReflection *ref = NULL;

	if(!self || !geometry || !detector)
		return NULL;

	ref = hkl_sample_reflection_new(geometry, detector, h, k, l);

	if(ref){
		self->reflections = realloc(self->reflections, sizeof(*self->reflections) * (self->reflections_len + 1));
		self->reflections[self->reflections_len++] = ref;
	}

	return ref;
}

/**
 * hkl_sample_get_ith_reflection: (skip)
 * @self:
 * @idx:
 *
 * get the ith reflection
 *
 * Returns:
 **/
HklSampleReflection* hkl_sample_get_ith_reflection(const HklSample *self, size_t idx)
{
	if (!self)
		return NULL;

	return self->reflections[idx];
}

/**
 * hkl_sample_del_reflection: (skip)
 * @self:
 * @idx:
 *
 * delete the idx reflection
 *
 * Returns:
 **/
int hkl_sample_del_reflection(HklSample *self, size_t idx)
{
	if (!self || (idx >= self->reflections_len))
		return HKL_FALSE;

	hkl_sample_reflection_free(self->reflections[idx]);
	self->reflections_len--;
	if(idx < self->reflections_len)
		memmove(&self->reflections[idx], &self->reflections[idx + 1],
			sizeof(*self->reflections) * (self->reflections_len - idx));

	return HKL_TRUE;
}

/**
 * hkl_sample_compute_UB_busing_levy: (skip)
 * @self:
 * @idx1:
 * @idx2:
 *
 * compute the UB matrix using the Busing and Levy method
 * add ref
 *
 * Returns:
 **/
int hkl_sample_compute_UB_busing_levy(HklSample *self, size_t idx1, size_t idx2)
{
	HklSampleReflection *r1;
	HklSampleReflection *r2;

	if (!self
	    || idx1 >= self->reflections_len
	    || idx2 >= self->reflections_len)
		return HKL_FALSE;

	r1 = self->reflections[idx1];
	r2 = self->reflections[idx2];

	if (!hkl_vector_is_colinear(&r1->hkl, &r2->hkl)) {
		HklVector h1c;
		HklVector h2c;
		HklMatrix B;
		HklMatrix Tc;

		/* Compute matrix Tc from r1 and r2. */
		h1c = r1->hkl;
		h2c = r2->hkl;
		hkl_lattice_get_B(self->lattice, &B);
		hkl_matrix_times_vector(&B, &h1c);
		hkl_matrix_times_vector(&B, &h2c);
		hkl_matrix_init_from_two_vector(&Tc, &h1c, &h2c);
		hkl_matrix_transpose(&Tc);

		/* compute U */
		hkl_matrix_init_from_two_vector(&self->U,
						&r1->_hkl, &r2->_hkl);
		hkl_matrix_times_matrix(&self->U, &Tc);
		hkl_sample_compute_UxUyUz(self);
		hkl_sample_compute_UB(self);
	} else
		return HKL_FALSE;

	return HKL_TRUE;
}

/**
 * hkl_sample_affine: (skip)
 * @self:
 *
 * affine the sample
 *
 * Returns:
 **/
double hkl_sample_affine(HklSample *self)
{
	if(!self)
		return GSL_NAN;

	return minimize(self, mono_crystal_fitness, self);
}

/**
 * hkl_sample_get_reflection_mesured_angle: (skip)
 * @self:
 * @idx1:
 * @idx2:
 *
 * get the mesured angles between two reflections
 *
 * Returns:
 **/
double hkl_sample_get_reflection_mesured_angle(const HklSample *self,
					       size_t idx1, size_t idx2)
{
	if (!self
	    || idx1 >= self->reflections_len
	    || idx2 >= self->reflections_len)
		return GSL_NAN;

	return hkl_vector_angle(&self->reflections[idx1]->_hkl,
				&self->reflections[idx2]->_hkl);
}

/**
 * hkl_sample_get_reflection_theoretical_angle: (skip)
 * @self:
 * @idx1:
 * @idx2:
 *
 * get the theoretical angles between two reflections
 *
 * Returns:
 **/
double hkl_sample_get_reflection_theoretical_angle(const HklSample *self,
						   size_t idx1, size_t idx2)
{
	HklVector hkl1;
	HklVector hkl2;

	if (!self
	    || idx1 >= self->reflections_len
	    || idx2 >= self->reflections_len)
		return GSL_NAN;

	hkl1 = self->reflections[idx1]->hkl;
	hkl2 = self->reflections[idx2]->hkl;
	hkl_matrix_times_vector(&self->UB, &hkl1);
	hkl_matrix_times_vector(&self->UB, &hkl2);

	return hkl_vector_angle(&hkl1, &hkl2);
}

/**
 * hkl_sample_fprintf: (skip)
 * @f:
 * @self:
 *
 * print to a file a sample
 **/
void hkl_sample_fprintf(FILE *f, const HklSample *self)
{
	size_t i, len;

	if(!self)
		return;

	fprintf(f, "\nSample name: \"%s\"", self->name);

	fprintf(f, "\nLattice parameters:");
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->a);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->b);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->c);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->alpha);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->beta);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->lattice->gamma);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->ux);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->uy);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->uz);
	fprintf(f, "\nUB:\n");
	hkl_matrix_fprintf(f, &self->UB);

	len = self->reflections_len;
	if (len){
		HklSampleReflection *reflection;
		HklParameter **axis;

		reflection  = hkl_sample_get_ith_reflection(self, 0);

		fprintf(f, "Reflections:");
		fprintf(f, "\n");
		fprintf(f, "i %-10.6s %-10.6s %-10.6s", "h", "k", "l");
		darray_foreach(axis, reflection->geometry->axes){
			fprintf(f, " %-10.6s", (*axis)->name);
		}

		for(i=0; i<len; ++i){
			reflection  = hkl_sample_get_ith_reflection(self, i);
			fprintf(f, "\n%d %-10.6f %-10.6f %-10.6f", i,
				reflection->hkl.data[0], reflection->hkl.data[1], reflection->hkl.data[2]);
			darray_foreach(axis, reflection->geometry->axes){
				fprintf(f, " %-10.6f", hkl_parameter_get_value_unit(*axis));
			}
		}
	}
}

/***********************/
/* HklSampleReflection */
/***********************/

/**
 * hkl_sample_reflection_new: (skip)
 * @geometry:
 * @detector:
 * @h:
 * @k:
 * @l:
 *
 * constructeur
 *
 * Returns:
 **/
HklSampleReflection *hkl_sample_reflection_new(HklGeometry *geometry,
					       const HklDetector *detector,
					       double h, double k, double l)
{
	HklSampleReflection *self;

	if (!geometry || !detector)
		return NULL;

	self = HKL_MALLOC(HklSampleReflection);

	hkl_geometry_update(geometry);

	self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = hkl_detector_new_copy(detector);
	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
	self->flag = HKL_TRUE;

	hkl_sample_reflection_update(self);

	return self;
}

/**
 * hkl_sample_reflection_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklSampleReflection *hkl_sample_reflection_new_copy(const HklSampleReflection *self)
{
	HklSampleReflection *dup = NULL;

	dup = HKL_MALLOC(HklSampleReflection);

	dup->geometry = hkl_geometry_new_copy(self->geometry);
	dup->detector = hkl_detector_new_copy(self->detector);
	dup->hkl = self->hkl;
	dup->_hkl = self->_hkl;
	dup->flag = self->flag;

	return dup;
}

/**
 * hkl_sample_reflection_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_sample_reflection_free(HklSampleReflection *self)
{
	hkl_geometry_free(self->geometry);
	hkl_detector_free(self->detector);
	free(self);
}

/**
 * hkl_sample_reflection_set_hkl: (skip)
 * @self:
 * @h:
 * @k:
 * @l:
 *
 * set the hkl value of the reflection
 **/
void hkl_sample_reflection_set_hkl(HklSampleReflection *self, double h, double k, double l)
{
	if(!self
	   || (fabs(h) + fabs(k) + fabs(l) < HKL_EPSILON))
		return;

	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
}

/**
 * hkl_sample_reflection_set_flag: (skip)
 * @self:
 * @flag:
 *
 * set the flag of the reglection
 **/
void hkl_sample_reflection_set_flag(HklSampleReflection *self, int flag)
{
	if(!self)
		return;
	self->flag = flag;
}

/**
 * hkl_sample_reflection_set_geometry: (skip)
 * @self:
 * @geometry:
 *
 * set the geometry of the reflection
 **/
void hkl_sample_reflection_set_geometry(HklSampleReflection *self, HklGeometry *geometry)
{
	if(!self || !geometry)
		return;

	if(self->geometry){
		if(self->geometry != geometry){
			hkl_geometry_free(self->geometry);
			self->geometry = hkl_geometry_new_copy(geometry);
		}
	}else
		self->geometry = hkl_geometry_new_copy(geometry);

	hkl_sample_reflection_update(self);
}

/*****************/
/* HklSampleList */
/*****************/

/**
 * hkl_sample_list_new: (skip)
 *
 * constructor
 *
 * Returns:
 **/
HklSampleList *hkl_sample_list_new(void)
{
	HklSampleList *self = NULL;
	self = HKL_MALLOC(HklSampleList);

	self->samples = NULL;
	self->len = 0;
	self->alloc = 0;
	self->current = NULL;

	return self;
}

/**
 * hkl_sample_list_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklSampleList *hkl_sample_list_new_copy(const HklSampleList *self)
{
	HklSampleList *dup;
	int i;

	dup = HKL_MALLOC(HklSampleList);

	*dup = *self;

	/* now copy the array of sample */
	dup->samples = NULL;
	dup->alloc = 0;
	ALLOC_GROW(dup->samples, self->len, dup->alloc);
	for(i=0; i<self->len; ++i)
		dup->samples[i] = hkl_sample_new_copy(self->samples[i]);

	hkl_sample_list_select_current(dup, self->current->name);

	return dup;
}

/**
 * hkl_sample_list_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_sample_list_free(HklSampleList *self)
{
	if (!self)
		return;

	hkl_sample_list_clear(self);
	if(self->alloc){
		free(self->samples);
		self->alloc = 0;
	}
	free(self);
}

/**
 * hkl_sample_list_clear: (skip)
 * @self:
 *
 * clear a sample list
 **/
void hkl_sample_list_clear(HklSampleList *self)
{
	size_t i;

	if(!self)
		return;

	for(i=0; i<self->len; ++i)
		hkl_sample_free(self->samples[i]);
	self->len = 0;
	self->current = NULL;
}

/**
 * hkl_sample_list_append: (skip)
 * @self:
 * @sample:
 *
 * append a sample to a sample list
 *
 * Returns:
 **/
HklSample *hkl_sample_list_append(HklSampleList *self, HklSample *sample)
{
	if (!self || !sample
	    || hkl_sample_list_get_idx_from_name(self, sample->name) >= 0)
		return NULL;

	ALLOC_GROW(self->samples, self->len + 1, self->alloc);
	self->samples[self->len++] = sample;

	return sample;
}

/**
 * hkl_sample_list_del: (skip)
 * @self:
 * @sample:
 *
 * remove a sample to the sample list
 **/
void hkl_sample_list_del(HklSampleList *self, HklSample *sample)
{
	size_t i;

	if(!self || !sample)
		return;

	for(i=0; i<self->len; ++i)
		if(self->samples[i] == sample){
			/* if the removed sample is the current sample set it to NULL */
			if(self->current == sample)
				self->current = NULL;
			/* remove it */
			hkl_sample_free(sample);
			self->len--;
			/* move all above sample of 1 position */
			if(i < self->len)
				memmove(&self->samples[i], &self->samples[i+1], sizeof(*self->samples) * (self->len - i));
		}
}

/**
 * hkl_sample_list_len: (skip)
 * @self:
 *
 * len of the sample list
 * @todo test and remove
 *
 * Returns:
 **/
size_t hkl_sample_list_len(const HklSampleList *self)
{
	if(!self)
		return -1;

	return self->len;
}

/**
 * hkl_sample_list_get_ith: (skip)
 * @self:
 * @idx:
 *
 * get the ith sample of the sample list
 * todo test
 *
 * Returns:
 **/
HklSample *hkl_sample_list_get_ith(HklSampleList *self, size_t idx)
{
	if(!self || idx >= self->len)
		return NULL;

	return self->samples[idx];
}

/**
 * hkl_sample_list_get_by_name: (skip)
 * @self: the #HklSampleList
 * @name: the name of the #HklSample you are looking for.
 *
 * get the @name named #HklSample from the #HklSampleList.
 *
 * Returns: an #HklSample or NULL if not present in the #HklSampleList
 *
 * todo: test method
 **/
HklSample *hkl_sample_list_get_by_name(HklSampleList *self, const char *name)
{
	HklSample *sample = NULL;
	int idx;

	if (!self || !name)
		return sample;

	idx = hkl_sample_list_get_idx_from_name(self, name);
	if (idx >= 0)
		sample = self->samples[idx];

	return sample;
}

/* TODO test */
/**
 * hkl_sample_list_get_idx_from_name: (skip)
 * @self: the #HklSampleList
 * @name: the name of the #HklSample.
 *
 * find the named @name #HklSample in the #HklSampleList and return
 * its index.
 *
 * Returns: the index or -1 if the #HklSample is not present.
 **/
int hkl_sample_list_get_idx_from_name(HklSampleList *self, const char *name)
{
	int idx = -1;

	if (!self || !name || !self->samples)
		return idx;

	for(idx=0; idx<self->len; ++idx)
		if (!strcmp(self->samples[idx]->name, name))
			return idx;

	return -1;
}

/**
 * hkl_sample_list_select_current: (skip)
 * @self:
 * @name:
 *
 * select the current sample of the sample list
 *
 * Returns:
 **/
int hkl_sample_list_select_current(HklSampleList *self, const char *name)
{
	int idx;

	if(!self || !name || !self->samples)
		return HKL_FALSE;

	idx = hkl_sample_list_get_idx_from_name(self, name);
	if (idx < 0)
		return HKL_FALSE;

	self->current = self->samples[idx];

	return HKL_TRUE;
}

/**
 * hkl_sample_list_fprintf: (skip)
 * @f:
 * @self:
 *
 * print the sample list to a file
 **/
void hkl_sample_list_fprintf(FILE *f, const HklSampleList *self)
{
	size_t i;
	for(i=0; i<self->len; ++i)
		hkl_sample_fprintf(f, self->samples[i]);
}
