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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

/* for strdup */
#define _XOPEN_SOURCE 500
#include <gsl/gsl_errno.h>              // for gsl_set_error_handler, etc
#include <gsl/gsl_multimin.h>           // for gsl_multimin_function, etc
#include <gsl/gsl_nan.h>                // for GSL_NAN
#include <gsl/gsl_vector_double.h>      // for gsl_vector_get, etc
#include <math.h>                       // for M_PI, fabs
#include <stddef.h>                     // for size_t
#include <stdio.h>                      // for fprintf, FILE
#include <stdlib.h>                     // for free
#include <string.h>                     // for NULL, strdup
#include "hkl-detector-private.h"       // for hkl_detector_new_copy, etc
#include "hkl-geometry-private.h"       // for _HklGeometry, etc
#include "hkl-lattice-private.h"        // for _HklLattice, etc
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-matrix-private.h"         // for hkl_matrix_init_from_euler, etc
#include "hkl-parameter-private.h"      // for hkl_parameter_fprintf, etc
#include "hkl-quaternion-private.h"     // for hkl_quaternion_conjugate, etc
#include "hkl-sample-private.h"         // for _HklSample, etc
#include "hkl-source-private.h"         // for hkl_source_compute_ki
#include "hkl-unit-private.h"           // for hkl_unit_angle_deg, etc
#include "hkl-vector-private.h"         // for HklVector, hkl_vector_angle, etc
#include "hkl.h"                        // for HklSample, etc
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, darray_item
#include "hkl/ccan/list/list.h"         // for list_head, list_add_tail, etc

/* #define DEBUG */
#define ITER_MAX 10000

/* private */
static void hkl_sample_clear_all_reflections(HklSample *self)
{
	HklSampleReflection *reflection;
	HklSampleReflection *next;

	list_for_each_safe(&self->reflections, reflection, next, list){
		list_del(&reflection->list);
		hkl_sample_reflection_free(reflection);
	}
}


static void hkl_sample_copy_all_reflections(HklSample *self, const  HklSample *src)
{
	HklSampleReflection *reflection;

	list_head_init(&self->reflections);
	list_for_each(&src->reflections, reflection, list){
		list_add_tail(&self->reflections,
			      &hkl_sample_reflection_new_copy(reflection)->list);
	}
	self->n_reflections = src->n_reflections;
}


static void hkl_sample_sample_set(HklSample *self, const HklSample *src)
{
	if(self->name)
		free(self->name);
	self->name = strdup(src->name);

	hkl_lattice_lattice_set(self->lattice, src->lattice);
	self->U = src->U;
	self->UB = src->UB;

	hkl_parameter_init_copy(self->ux, src->ux, NULL);
	hkl_parameter_init_copy(self->uy, src->uy, NULL);
	hkl_parameter_init_copy(self->uz, src->uz, NULL);

	/* copy all the reflections */
	hkl_sample_clear_all_reflections(self);
	hkl_sample_copy_all_reflections(self, src);
}


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
	hkl_parameter_value_set(self->ux, ux, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uy, uy, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uz, uz, HKL_UNIT_DEFAULT, NULL);
}

static int hkl_sample_compute_UB(HklSample *self)
{
	HklMatrix B;

	if (!hkl_lattice_get_B(self->lattice, &B))
		return FALSE;

	self->UB = self->U;
	hkl_matrix_times_matrix(&self->UB, &B);

	return TRUE;
}

/*
 * this structure is used by the minimization gsl algorithm.
 * in the set_UB method
 */
struct set_UB_t
{
	HklSample *sample;
	const HklMatrix UB;
};

static int hkl_sample_init_from_gsl_vector(HklSample *self, const gsl_vector *x)
{
	double euler_x, euler_y, euler_z;

	euler_x = gsl_vector_get(x, 0);
	euler_y = gsl_vector_get(x, 1);
	euler_z = gsl_vector_get(x, 2);

	hkl_parameter_value_set(self->ux, euler_x, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uy, euler_y, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uz, euler_z, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->a, gsl_vector_get(x, 3), HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->b, gsl_vector_get(x, 4), HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->c, gsl_vector_get(x, 5), HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->alpha, gsl_vector_get(x, 6), HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->beta, gsl_vector_get(x, 7), HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->lattice->gamma, gsl_vector_get(x, 8), HKL_UNIT_DEFAULT, NULL);

	hkl_matrix_init_from_euler(&self->U, euler_x, euler_y, euler_z);
	if (!hkl_sample_compute_UB(self))
		return FALSE;

	return TRUE;
}

static void hkl_sample_to_gsl_vector(HklSample *self, gsl_vector *x)
{
	gsl_vector_set (x, 0, hkl_parameter_value_get(self->ux, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 1, hkl_parameter_value_get(self->uy, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 2, hkl_parameter_value_get(self->uz, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 3, hkl_parameter_value_get(self->lattice->a, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 4, hkl_parameter_value_get(self->lattice->b, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 5, hkl_parameter_value_get(self->lattice->c, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 6, hkl_parameter_value_get(self->lattice->alpha, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 7, hkl_parameter_value_get(self->lattice->beta, HKL_UNIT_DEFAULT));
	gsl_vector_set (x, 8, hkl_parameter_value_get(self->lattice->gamma, HKL_UNIT_DEFAULT));

}

static double set_UB_fitness(const gsl_vector *x, void *params)
{
	size_t i, j;
	double fitness = 0.;
	struct set_UB_t *parameters = params;
	HklSample *sample = parameters->sample;

	if (!hkl_sample_init_from_gsl_vector(sample, x))
		return GSL_NAN;

	for(i=0; i<3; ++i)
		for(j=0; j<3; ++j){
			double tmp = parameters->UB.data[i][j] - sample->UB.data[i][j];
			fitness += tmp * tmp;
		}
#ifdef DEBUG
	fprintf(stderr, "fitness: %f\n", fitness);
#endif
	return fitness;
}

static double mono_crystal_fitness(const gsl_vector *x, void *params)
{
	size_t i, j;
	double fitness;
	HklSample *sample = params;
	HklSampleReflection *reflection;

	if (!hkl_sample_init_from_gsl_vector(sample, x))
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

static int minimize(HklSample *sample,
		    double (* f) (const gsl_vector * x, void * params),
		    void *params, GError **error)
{
	HklSample *saved;
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size = 0;
	int res = TRUE;

	hkl_error (error == NULL || *error == NULL);

	/* save the sample state */
	saved = hkl_sample_new_copy(sample);

	/* Starting point */
	x = gsl_vector_alloc (9);
	hkl_sample_to_gsl_vector(sample, x);

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
#ifdef DEBUG
		fprintf(stderr, "status iterate: %d (%d): %s\n", status, iter, gsl_strerror(status));
#endif
		if (status)
			break;
		size = gsl_multimin_fminimizer_size (s);
		status = gsl_multimin_test_size (size, HKL_EPSILON / 2.);
#ifdef DEBUG
		fprintf(stderr, "status test: %d size: %f :%s\n", status, size, gsl_strerror(status));
		fprintf(stderr, " x:");
		for(int i=0; i<9; ++i)
			fprintf(stderr, " %f", gsl_vector_get(s->x, i));
		fprintf(stderr, "\n");
#endif
	} while (status == GSL_CONTINUE && iter < ITER_MAX);
	gsl_vector_free(x);
	gsl_vector_free(ss);
	gsl_multimin_fminimizer_free(s);
	gsl_set_error_handler (NULL);

	if (status == GSL_CONTINUE){
		hkl_sample_sample_set(sample, saved); /* restore the saved sample */
		g_set_error(error,
			    HKL_SAMPLE_ERROR,
			    HKL_SAMPLE_ERROR_MINIMIZED,
			    "Minimization failed after %d iterations.",
			    ITER_MAX);
		res = FALSE;
	}

	hkl_sample_free(saved);

	return res;
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

	self->ux = hkl_parameter_new("ux", "the sample rotation around $\vec{x}$",
				     -M_PI, 0., M_PI,
				     TRUE, TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);
	self->uy = hkl_parameter_new("uy", "the sample rotation around $\vec{y}$",
				     -M_PI, 0., M_PI,
				     TRUE, TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);
	self->uz = hkl_parameter_new("uz", "the sample rotation around $\vec{z}$",
				     -M_PI, 0., M_PI,
				     TRUE, TRUE,
				     &hkl_unit_angle_rad,
				     &hkl_unit_angle_deg);

	hkl_sample_compute_UB(self);
	list_head_init(&self->reflections);
	self->n_reflections = 0;

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

	hkl_sample_copy_all_reflections(dup, self);

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
	if (!self)
		return;

	free(self->name);
	hkl_lattice_free(self->lattice);
	hkl_parameter_free(self->ux);
	hkl_parameter_free(self->uy);
	hkl_parameter_free(self->uz);
	hkl_sample_clear_all_reflections(self);
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
 * hkl_sample_name_set:
 * @self: the this ptr
 * @name: the new name to set
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
 * Return value: the lattice parameters of the sample.
 **/
const HklLattice *hkl_sample_lattice_get(HklSample *self)
{
	return self->lattice;
}

/**
 * hkl_sample_lattice_set:
 * @self: the this ptr
 * @lattice: the lattice to set
 **/
void hkl_sample_lattice_set(HklSample *self, const HklLattice *lattice)
{
	hkl_lattice_lattice_set(self->lattice, lattice);
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_ux_get:
 * @self: the this ptr
 *
 * Return value: the ux part of the U matrix.
 **/
const HklParameter *hkl_sample_ux_get(const HklSample *self)
{
	return self->ux;
}

/**
 * hkl_sample_uy_get:
 * @self: the this ptr
 *
 * Return value: the uy part of the U matrix.
 **/
const HklParameter *hkl_sample_uy_get(const HklSample *self)
{
	return self->uy;
}

/**
 * hkl_sample_uz_get:
 * @self: the this ptr
 *
 * Return value: the uz part of the U matrix.
 **/
const HklParameter *hkl_sample_uz_get(const HklSample *self)
{
	return self->uz;
}

/**
 * hkl_sample_ux_set:
 * @self: the this ptr
 * @ux: the ux parameter to set
 * @error: return location for a GError, or NULL
 *
 * set the ux part of the U matrix.
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_sample_ux_set(HklSample *self,
		      const HklParameter *ux,
		      GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!hkl_parameter_init_copy(self->ux, ux, error)){
		g_assert (error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uy, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uz, HKL_UNIT_DEFAULT));
	hkl_sample_compute_UB(self);

	return TRUE;
}

/**
 * hkl_sample_uy_set:
 * @self: the this ptr
 * @uy: the uy parameter to set
 * @error: return location for a GError, or NULL
 *
 * set the uy part of the U matrix.
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_sample_uy_set(HklSample *self, const HklParameter *uy,
		      GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!hkl_parameter_init_copy(self->uy, uy, error)){
		g_assert (error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uy, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uz, HKL_UNIT_DEFAULT));
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_uz_set:
 * @self: the this ptr
 * @uz: the uz parameter to set
 * @error: return location for a GError, or NULL
 *
 * set the uz part of the U matrix.
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_sample_uz_set(HklSample *self, const HklParameter *uz,
		      GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!hkl_parameter_init_copy(self->uz, uz, error)){
		g_assert (error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_matrix_init_from_euler(&self->U,
				   hkl_parameter_value_get(self->ux, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uy, HKL_UNIT_DEFAULT),
				   hkl_parameter_value_get(self->uz, HKL_UNIT_DEFAULT));
	hkl_sample_compute_UB(self);
}

/**
 * hkl_sample_U_get:
 * @self: the this ptr
 *
 * Return value: the U matrix of the sample
 **/
const HklMatrix *hkl_sample_U_get(const HklSample *self)
{
	return &self->U;
}

/*
 * TODO implemente the error
 */
void hkl_sample_U_set(HklSample *self, const HklMatrix *U, GError **error)
{
	double x, y, z;

	hkl_matrix_matrix_set(&self->U, U);
	hkl_sample_compute_UB(self);
	hkl_matrix_to_euler(&self->U, &x, &y, &z);
	hkl_parameter_value_set(self->ux, x, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uy, y, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(self->uz, z, HKL_UNIT_DEFAULT, NULL);
}

/**
 * hkl_sample_UB_get:
 * @self: the this ptr.
 *
 * Return value: get the UB matrix of the sample
 **/
const HklMatrix *hkl_sample_UB_get(const HklSample *self)
{
	return &self->UB;
}

/**
 * hkl_sample_UB_set:
 * @self: the sample to modify
 * @UB: the UB matrix to set
 * @error: error set in case of impossibility
 *
 * Set the UB matrix using an external UB matrix. In fact you give
 * the UB matrix but only the U matrix of the sample is affected by
 * this operation. We keep the B matrix constant.
 * U * B = UB -> U = UB * B^-1
 * TODO implemente the error
 **/
int hkl_sample_UB_set(HklSample *self, const HklMatrix *UB,
		      GError **error)
{
	struct set_UB_t params = {
		.sample = self,
		.UB = *UB
	};

	return minimize(self, set_UB_fitness, &params, error);
}

/**
 * hkl_sample_n_reflections_get: (skip)
 * @self: the this ptr
 *
 * return the number of reflections of the sample
 *
 * Returns:
 **/
size_t hkl_sample_n_reflections_get(const HklSample *self)
{
	return self->n_reflections;
}

/**
 * hkl_sample_reflections_first_get: (skip)
 * @self: the this ptr
 *
 * Return value: the first HklSampleReflection of the sample.
 **/
HklSampleReflection *hkl_sample_reflections_first_get(HklSample *self)
{
	return list_top(&self->reflections, HklSampleReflection, list);
}

/**
 * hkl_sample_reflections_next_get: (skip)
 * @self: the this ptr
 * @reflection: the current reflection
 *
 * Return value: (allow-none): the next reflection or NULL if no more reflection
 **/
HklSampleReflection *hkl_sample_reflections_next_get(HklSample *self,
						     HklSampleReflection *reflection)
{
	return list_next(&self->reflections, reflection, list);
}

/**
 * hkl_sample_add_reflection:
 * @self: the this ptr
 * @reflection: The reflection to add
 *
 * add a reflection to the sample, if the reflection is already part
 * of the sample reflection list, this method does nothing.
 **/
void hkl_sample_add_reflection(HklSample *self,
			       HklSampleReflection *reflection)
{
	HklSampleReflection *ref;

	list_for_each(&self->reflections, ref, list){
		if (ref == reflection)
			return;
	}

	list_add_tail(&self->reflections, &reflection->list);
	self->n_reflections++;
}

/**
 * hkl_sample_del_reflection:
 * @self: the this ptr
 * @reflection: the reflection to remove.
 *
 * remove an HklSampleRefelction from the reflections list.
 **/
void hkl_sample_del_reflection(HklSample *self,
			       HklSampleReflection *reflection)
{
	list_del(&reflection->list);
	hkl_sample_reflection_free(reflection);
	self->n_reflections--;
}

/**
 * hkl_sample_compute_UB_busing_levy:
 * @self: the this ptr
 * @r1: the first #HklsampleReflection
 * @r2: the second #HklSampleReflection
 *
 * compute the UB matrix using the Busing and Levy method
 * #todo: add ref
 *
 * Returns: 0 or 1 if it succeed
 **/
int hkl_sample_compute_UB_busing_levy(HklSample *self,
				      const HklSampleReflection *r1,
				      const HklSampleReflection *r2,
				      GError **error)

{
	hkl_error (error == NULL || *error == NULL);

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
	}else{
		g_set_error(error,
			    HKL_SAMPLE_ERROR,
			    HKL_SAMPLE_ERROR_COMPUTE_UB_BUSING_LEVY,
			    "It is not possible to compute the UB matrix when the given reflections are colinear");

		return FALSE;
	}
	g_assert (error == NULL || *error == NULL);

	return TRUE;
}

/**
 * hkl_sample_affine:
 * @self: the this ptr
 *
 * affine the sample
 *
 * Returns: the fitness of the affined #HklSample
 **/
int hkl_sample_affine(HklSample *self, GError **error)
{
	return minimize(self, mono_crystal_fitness, self, error);
}

/**
 * hkl_sample_get_reflection_measured_angle:
 * @self: the this ptr
 * @r1: the first #HklSampleReflection
 * @r2: the second #HklSampleReflection
 *
 * get the measured angles between two #HklSampleReflection
 *
 * Returns: the measured angle beetween them
 **/
double hkl_sample_get_reflection_measured_angle(const HklSample *self,
					       const HklSampleReflection *r1,
					       const HklSampleReflection *r2)
{
	return hkl_vector_angle(&r1->_hkl,
				&r2->_hkl);
}

/**
 * hkl_sample_get_reflection_theoretical_angle:
 * @self: the this ptr
 * @r1: the first #HklSampleReflection
 * @r2: the second #HklSampleReflection
 *
 * get the theoretical angles between two #HklSampleReflection
 *
 * Returns: the theoretical angle beetween them
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
				fprintf(f, " %-10.6f", hkl_parameter_value_get(*axis, HKL_UNIT_USER));
			}
		}
	}
}

/***********************/
/* HklSampleReflection */
/***********************/

/**
 * hkl_sample_reflection_new:
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
HklSampleReflection *hkl_sample_reflection_new(const HklGeometry *geometry,
					       const HklDetector *detector,
					       double h, double k, double l,
					       GError **error)
{
	HklSampleReflection *self;

	if (!geometry || !detector)
		return NULL;

	self = HKL_MALLOC(HklSampleReflection);

	self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = hkl_detector_new_copy(detector);
	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
	self->flag = TRUE;

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
 * hkl_sample_reflection_hkl_get:
 * @self: the this ptr
 * @h: (out caller-allocates): the h-coordinate of the #HklSampleReflection
 * @k: (out caller-allocates): the k-coordinate of the #HklSampleReflection
 * @l: (out caller-allocates): the l-coordinate of the #HklSampleReflection
 *
 * get the hkl coordinates of the #HklSampleReflection
 **/
void hkl_sample_reflection_hkl_get(const HklSampleReflection *self,
				   double *h, double *k, double *l)
{
	*h = self->hkl.data[0];
	*k = self->hkl.data[1];
	*l = self->hkl.data[2];
}

/**
 * hkl_sample_reflection_hkl_set:
 * @self: the this ptr
 * @h: the h-coordinate of the #HklSampleReflection
 * @k: the k-coordinate of the #HklSampleReflection
 * @l: the l-coordinate of the #HklSampleReflection
 * @error: return location for a GError, or NULL
 *
 * set the hkl coordinates of the #HklSampleReflection
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_sample_reflection_hkl_set(HklSampleReflection *self,
				  double h, double k, double l,
				  GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if((fabs(h) + fabs(k) + fabs(l) < HKL_EPSILON)){
		g_set_error(error,
			    HKL_SAMPLE_REFLECTION_ERROR,
			    HKL_SAMPLE_REFLECTION_ERROR_HKL_SET,
			    "it is not allow to set a null hkl reflection\n");
		return FALSE;
	}

	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;

	return TRUE;
}

/**
 * hkl_sample_reflection_flag_get:
 * @self: the this ptr
 *
 * get the flag of the reflection
 *
 * Returns: the flag value
 **/
int hkl_sample_reflection_flag_get(const HklSampleReflection *self)
{
	return self->flag;
}

/**
 * hkl_sample_reflection_flag_set:
 * @self: the this ptr
 * @flag: the value of the flag to set
 *
 * set the flag of the reflection.
 **/
void hkl_sample_reflection_flag_set(HklSampleReflection *self, int flag)
{
	self->flag = flag;
}

/**
 * hkl_sample_reflection_geometry_get:
 * @self: the this ptr
 *
 * get the #HklGeometry stored in the #HklSampleReflection
 *
 * Returns: the geometry saved into the reflection.
 **/
const HklGeometry *hkl_sample_reflection_geometry_get(HklSampleReflection *self)
{
	return self->geometry;
}

/**
 * hkl_sample_reflection_geometry_set:
 * @self: the this ptr
 * @geometry: the geometry to set in the  #HklSampleReflection
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
