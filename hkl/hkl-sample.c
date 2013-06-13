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

#include <hkl/hkl-matrix.h>

#include "hkl-axis-private.h"
#include "hkl-detector-private.h"
#include "hkl-parameter-private.h"
#include "hkl-geometry-private.h"
#include "hkl-sample-private.h"

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
	hkl_parameter_value_set(self->ux, ux, NULL);
	hkl_parameter_value_set(self->uy, uy, NULL);
	hkl_parameter_value_set(self->uz, uz, NULL);
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

	hkl_parameter_value_set(sample->ux, euler_x, NULL);
	hkl_parameter_value_set(sample->uy, euler_y, NULL);
	hkl_parameter_value_set(sample->uz, euler_z, NULL);
	hkl_parameter_value_set(sample->lattice->a, gsl_vector_get(x, 3), NULL);
	hkl_parameter_value_set(sample->lattice->b, gsl_vector_get(x, 4), NULL);
	hkl_parameter_value_set(sample->lattice->c, gsl_vector_get(x, 5), NULL);
	hkl_parameter_value_set(sample->lattice->alpha, gsl_vector_get(x, 6), NULL);
	hkl_parameter_value_set(sample->lattice->beta, gsl_vector_get(x, 7), NULL);
	hkl_parameter_value_set(sample->lattice->gamma, gsl_vector_get(x, 8), NULL);

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
	HklSampleReflection *reflection;

	euler_x = gsl_vector_get(x, 0);
	euler_y = gsl_vector_get(x, 1);
	euler_z = gsl_vector_get(x, 2);

	hkl_parameter_value_set(sample->ux, euler_x, NULL);
	hkl_parameter_value_set(sample->uy, euler_y, NULL);
	hkl_parameter_value_set(sample->uz, euler_z, NULL);
	hkl_parameter_value_set(sample->lattice->a, gsl_vector_get(x, 3), NULL);
	hkl_parameter_value_set(sample->lattice->b, gsl_vector_get(x, 4), NULL);
	hkl_parameter_value_set(sample->lattice->c, gsl_vector_get(x, 5), NULL);
	hkl_parameter_value_set(sample->lattice->alpha, gsl_vector_get(x, 6), NULL);
	hkl_parameter_value_set(sample->lattice->beta, gsl_vector_get(x, 7), NULL);
	hkl_parameter_value_set(sample->lattice->gamma, gsl_vector_get(x, 8), NULL);
	hkl_matrix_init_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (!hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	list_for_each(&sample->reflections, reflection, list){
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
	gsl_vector_set (x, 0, hkl_parameter_value_get(sample->ux));
	gsl_vector_set (x, 1, hkl_parameter_value_get(sample->uy));
	gsl_vector_set (x, 2, hkl_parameter_value_get(sample->uz));
	gsl_vector_set (x, 3, hkl_parameter_value_get(sample->lattice->a));
	gsl_vector_set (x, 4, hkl_parameter_value_get(sample->lattice->b));
	gsl_vector_set (x, 5, hkl_parameter_value_get(sample->lattice->c));
	gsl_vector_set (x, 6, hkl_parameter_value_get(sample->lattice->alpha));
	gsl_vector_set (x, 7, hkl_parameter_value_get(sample->lattice->beta));
	gsl_vector_set (x, 8, hkl_parameter_value_get(sample->lattice->gamma));

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
 *
 * constructor
 *
 * Returns:
 **/
HklSample* hkl_sample_new(const char *name)
{
	HklSample *self = NULL;

	/* check parameters */
	if(!name)
		return self;

	self = HKL_MALLOC(HklSample);

	self->name = strdup(name);
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
	list_head_init(&self->reflections);

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
	HklSampleReflection *reflection;

	/* check parameters */
	if(!self)
		return dup;

	dup = HKL_MALLOC(HklSample);

	dup->name = strdup(self->name);
	dup->lattice = hkl_lattice_new_copy(self->lattice);
	dup->U = self->U;
	dup->UB = self->UB;
	dup->ux = hkl_parameter_new_copy(self->ux);
	dup->uy = hkl_parameter_new_copy(self->uy);
	dup->uz = hkl_parameter_new_copy(self->uz);

	/* copy the reflections */
	list_head_init(&dup->reflections);
	list_for_each(&self->reflections, reflection, list){
		list_add_tail(&dup->reflections,
			      &hkl_sample_reflection_new_copy(reflection)->list);
	}

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
	HklSampleReflection *reflection;
	HklSampleReflection *next;

	if (!self)
		return;

	free(self->name);
	hkl_lattice_free(self->lattice);
	hkl_parameter_free(self->ux);
	hkl_parameter_free(self->uy);
	hkl_parameter_free(self->uz);
	list_for_each_safe(&self->reflections, reflection, next, list){
		list_del(&reflection->list);
		hkl_sample_reflection_free(reflection);
	}
	free(self);
}

/**
 * hkl_sample_name_get:
 * @self: the this ptr
 *
 * Return value: the name of the sample
 **/
const char *hkl_sample_name_get(const HklSample *self)
{
	return self->name;
}

/**
 * hkl_sample_name_set: (skip)
 * @self:
 * @name:
 *
 * set the name of the sample
 **/
void hkl_sample_name_set(HklSample *self, const char *name)
{
	if(self->name)
		free(self->name);
	self->name = strdup(name);
}

/**
 * hkl_sample_lattice_get:
 * @self: the this ptr
 *
 * Return value: (transfer none): the lattice parameters of the sample.
 **/
HklLattice *hkl_sample_lattice_get(HklSample *self)
{
	return self->lattice;
}

/**
 * hkl_sample_lattice_set:
 * @self: the this ptr
 * @lattice: (transfer none): the lattice to set
 **/
void hkl_sample_lattice_set(HklSample *self, HklLattice *lattice)
{
	hkl_lattice_lattice_set(self->lattice, lattice);
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_ux_get: (skip)
 * @self: the this ptr
 *
 * Return value: the ux part of the U matrix.
 **/
HklParameter *hkl_sample_ux_get(const HklSample *self)
{
	return self->ux;
}

/**
 * hkl_sample_uy_get: (skip)
 * @self: the this ptr
 *
 * Return value: the uy part of the U matrix.
 **/
HklParameter *hkl_sample_uy_get(const HklSample *self)
{
	return self->uy;
}

/**
 * hkl_sample_uz_get: (skip)
 * @self: the this ptr
 *
 * Return value: the uz part of the U matrix.
 **/
HklParameter *hkl_sample_uz_get(const HklSample *self)
{
	return self->uz;
}

/**
 * hkl_sample_ux_set: (skip)
 * @self: the this ptr
 * @ux: the ux parameter to set

 * set the ux part of the U matrix.
 **/
void hkl_sample_ux_set(HklSample *self,
		       const HklParameter *ux)
{
	hkl_parameter_value_set(self->ux,
				hkl_parameter_value_get(ux),
				NULL);
	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux),
				   hkl_parameter_value_get(self->uy),
				   hkl_parameter_value_get(self->uz));
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_uy_set: (skip)
 * @self: the this ptr
 * @uy: the uy parameter to set

 * set the uy part of the U matrix.
 **/
void hkl_sample_uy_set(HklSample *self,
		       const HklParameter *uy)
{
	hkl_parameter_value_set(self->uy,
				hkl_parameter_value_get(uy),
				NULL);
	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux),
				   hkl_parameter_value_get(self->uy),
				   hkl_parameter_value_get(self->uz));
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_uz_set: (skip)
 * @self: the this ptr
 * @uz: the uz parameter to set

 * set the uz part of the U matrix.
 **/
void hkl_sample_uz_set(HklSample *self,
		       const HklParameter *uz)
{
	hkl_parameter_value_set(self->uz,
				hkl_parameter_value_get(uz),
				NULL);
	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux),
				   hkl_parameter_value_get(self->uy),
				   hkl_parameter_value_get(self->uz));
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_U_get: (skip)
 * @self: the this ptr
 *
 * Return value: the U matrix of the sample
 **/
const HklMatrix *hkl_sample_U_get(const HklSample *self)
{
	return &self->U;
}

void hkl_sample_U_set(HklSample *self, const HklMatrix *U)
{
	double x, y, z;

	hkl_matrix_matrix_set(&self->U, U);
	hkl_sample_compute_UB(self);
	hkl_matrix_to_euler(&self->U, &x, &y, &z);
	hkl_parameter_value_set(self->ux, x, NULL);
	hkl_parameter_value_set(self->uy, y, NULL);
	hkl_parameter_value_set(self->uz, z, NULL);
}

/**
 * hkl_sample_UB_get: (skip)
 * @self:
 *
 * Return value: get the UB matrix of the sample
 **/
const HklMatrix *hkl_sample_UB_get(const HklSample *self)
{
	return &self->UB;
}

/**
 * hkl_sample_UB_set: (skip)
 * @self: the sample to modify
 * @UB: (in): the UB matrix to set
 *
 * Set the UB matrix using an external UB matrix. In fact you give
 * the UB matrix but only the U matrix of the sample is affected by
 * this operation. We keep the B matrix constant.
 * U * B = UB -> U = UB * B^-1
 **/
double hkl_sample_UB_set(HklSample *self, const HklMatrix *UB)
{
	struct set_UB_t params = {
		.sample = self,
		.UB = UB
	};

	return minimize(self, set_UB_fitness, &params);
}

HklSampleReflection *hkl_sample_first_reflection_get(const HklSample *self)
{
	return list_top(&self->reflections, HklSampleReflection, list);
}

HklSampleReflection *hkl_sample_next_reflection_get(const HklSample *self,
						    const HklSampleReflection *reflection)
{
	if (&self->reflections.n == reflection->list.next)
		return NULL;
	else
		return list_entry(reflection->list.next, HklSampleReflection, list);
}

/**
 * hkl_sample_add_reflection: (skip)
 * @self:
 * @reflection:
 *
 * add a reflection to the sample
 *
 * Returns: the added HklSampleReflection
 **/
void hkl_sample_add_reflection(HklSample *self,
					       HklSampleReflection *reflection)
{
	list_add_tail(&self->reflections, &reflection->list);
}

/**
 * hkl_sample_del_reflection: (skip)
 * @reflection: the reflection to remove.
 *
 * remove an HklSampleRefelction from the relfections list.
 **/
void hkl_sample_del_reflection(HklSampleReflection *reflection)
{
	list_del(&reflection->list);
	hkl_sample_reflection_free(reflection);
}

/**
 * hkl_sample_compute_UB_busing_levy: (skip)
 * @self:
 * @r1:
 * @r2:
 *
 * compute the UB matrix using the Busing and Levy method
 * add ref
 *
 * Returns:
 **/
int hkl_sample_compute_UB_busing_levy(HklSample *self,
				      const HklSampleReflection *r1,
				      const HklSampleReflection *r2)

{
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
	return minimize(self, mono_crystal_fitness, self);
}

/**
 * hkl_sample_get_reflection_mesured_angle: (skip)
 * @self: the this ptr
 * @r1: the first reflection
 * @r2: the second reflection
 *
 * get the mesured angles between two reflections
 *
 * Returns:
 **/
double hkl_sample_get_reflection_mesured_angle(const HklSample *self,
					       const HklSampleReflection *r1,
					       const HklSampleReflection *r2)
{
	return hkl_vector_angle(&r1->_hkl,
				&r2->_hkl);
}

/**
 * hkl_sample_get_reflection_theoretical_angle: (skip)
 * @self: the this ptr
 * @r1: the first reflection
 * @r2: the second reflection
 *
 * get the theoretical angles between two reflections
 *
 * Returns:
 **/
double hkl_sample_get_reflection_theoretical_angle(const HklSample *self,
						   const HklSampleReflection *r1,
						   const HklSampleReflection *r2)
{
	HklVector hkl1;
	HklVector hkl2;

	hkl1 = r1->hkl;
	hkl2 = r2->hkl;
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

	if (!list_empty(&self->reflections)){
		HklSampleReflection *reflection;
		HklParameter **axis;

		reflection  = list_top(&self->reflections, HklSampleReflection, list);

		fprintf(f, "Reflections:");
		fprintf(f, "\n");
		fprintf(f, "%-10.6s %-10.6s %-10.6s", "h", "k", "l");
		darray_foreach(axis, reflection->geometry->axes){
			fprintf(f, " %-10.6s", (*axis)->name);
		}

		list_for_each(&self->reflections, reflection, list){
			fprintf(f, "\n%-10.6f %-10.6f %-10.6f",
				reflection->hkl.data[0],
				reflection->hkl.data[1],
				reflection->hkl.data[2]);
			darray_foreach(axis, reflection->geometry->axes){
				fprintf(f, " %-10.6f", hkl_parameter_value_unit_get(*axis));
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
 * hkl_sample_reflection_hkl_get: (skip)
 * @self:
 * @h:
 * @k:
 * @l:
 *
 * get the hkl value of the reflection
 **/
void hkl_sample_reflection_hkl_get(const HklSampleReflection *self,
				   double *h, double *k, double *l)
{
	*h = self->hkl.data[0];
	*k = self->hkl.data[1];
	*l = self->hkl.data[2];
}

/**
 * hkl_sample_reflection_hkl_set: (skip)
 * @self:
 * @h:
 * @k:
 * @l:
 *
 * set the hkl value of the reflection
 **/
void hkl_sample_reflection_hkl_set(HklSampleReflection *self, double h, double k, double l)
{
	if((fabs(h) + fabs(k) + fabs(l) < HKL_EPSILON))
		return;

	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
}

/**
 * hkl_sample_reflection_flag_get: (skip)
 * @self:
 *
 * get the flag of the reflection
 **/
bool hkl_sample_reflection_flag_get(const HklSampleReflection *self)
{
	return self->flag;
}

/**
 * hkl_sample_reflection_flag_set: (skip)
 * @self:
 * @flag:
 *
 * set the flag of the reglection
 **/
void hkl_sample_reflection_flag_set(HklSampleReflection *self, bool flag)
{
	self->flag = flag;
}

/**
 * hkl_sample_reflection_geometry_get: (skip)
 * @self:
 *
 * set the geometry of the reflection
 **/
HklGeometry *hkl_sample_reflection_geometry_get(HklSampleReflection *self)
{
	return self->geometry;
}

/**
 * hkl_sample_reflection_geometry_set: (skip)
 * @self:
 * @geometry:
 *
 * set the geometry of the reflection
 **/
void hkl_sample_reflection_geometry_set(HklSampleReflection *self,
					const HklGeometry *geometry)
{
	if(self->geometry){
		if(self->geometry != geometry){
			hkl_geometry_free(self->geometry);
			self->geometry = hkl_geometry_new_copy(geometry);
		}
	}else
		self->geometry = hkl_geometry_new_copy(geometry);

	hkl_sample_reflection_update(self);
}
