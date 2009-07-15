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
#include <string.h>
#include <gsl/gsl_multimin.h>

#include <hkl/hkl-sample.h>
#include <hkl/hkl-matrix.h>

/* private */

static HklSampleReflection *hkl_sample_reflection_new(HklGeometry *geometry,
						      HklDetector const *detector,
						      double h, double k, double l)
{
	HklSampleReflection *self;
	HklVector ki;
	HklQuaternion q;

	if (!geometry || !detector)
		return NULL;

	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate memory for an HklSampleReflection");

	hkl_geometry_update(geometry);

	self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = *detector;
	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
	self->flag = HKL_TRUE;

	// compute the _hkl using only the axes of the geometry
	// first Q from angles
	hkl_source_compute_ki(&geometry->source, &ki);
	self->_hkl = ki;
	hkl_vector_rotated_quaternion(&self->_hkl, &geometry->holders[detector->idx].q);
	hkl_vector_minus_vector(&self->_hkl, &ki);

	q = geometry->holders[0].q;
	hkl_quaternion_conjugate(&q);
	hkl_vector_rotated_quaternion(&self->_hkl, &q);

	return self;
}

static HklSampleReflection *hkl_sample_reflection_new_copy(HklSampleReflection const *src)
{
	HklSampleReflection *self = NULL;

	self = malloc(sizeof(*self));
	if (!self)
		die("Can not allocate memory for a HklSampleReflection");

	self->geometry = hkl_geometry_new_copy(src->geometry);
	self->detector = src->detector;
	self->hkl = src->hkl;
	self->_hkl = src->_hkl;
	self->flag = src->flag;

	return self;
}

static void hkl_sample_reflection_free(HklSampleReflection *self)
{
	hkl_geometry_free(self->geometry);
	free(self);
}

static int hkl_sample_compute_UB(HklSample *self)
{
	HklMatrix B;

	if (hkl_lattice_get_B(self->lattice, &B))
		return HKL_FAIL;

	self->UB = self->U;
	hkl_matrix_times_smatrix(&self->UB, &B);

	return HKL_SUCCESS;
}

static double mono_crystal_fitness(gsl_vector const *x, void *params)
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
	sample->lattice->a->value = gsl_vector_get(x, 3);
	sample->lattice->b->value = gsl_vector_get(x, 4);
	sample->lattice->c->value = gsl_vector_get(x, 5);
	sample->lattice->alpha->value = gsl_vector_get(x, 6);
	sample->lattice->beta->value = gsl_vector_get(x, 7);
	sample->lattice->gamma->value = gsl_vector_get(x, 8);
	hkl_matrix_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i) {
		HklVector UBh;

		UBh = sample->reflections[i]->hkl;
		hkl_matrix_times_vector(&sample->UB, &UBh);

		for(j=0; j<3; ++j) {
			double tmp = UBh.data[j] - sample->reflections[i]->_hkl.data[j];
			fitness += tmp * tmp;
		}
	}
	return fitness;
}

/*************/
/* HklSample */
/*************/

HklSample* hkl_sample_new(char const *name, HklSampleType type)
{
	HklSample *self = NULL;

	// check parameters
	if(!name)
		return self;

	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate memory for a Sample");

	self->name = strdup(name);
	self->type = type;
	self->lattice = hkl_lattice_new_default();
	hkl_matrix_init(&self->U,1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_matrix_init(&self->UB,1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_sample_compute_UB(self);
	HKL_LIST_INIT(self->reflections);

	return self;
}

HklSample *hkl_sample_new_copy(HklSample const *src)
{
	HklSample *self = NULL;
	size_t len;
	size_t i;

	// check parameters
	if(!src)
		return self;

	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate memory for a Sample");

	self->name = strdup(src->name);
	self->type = src->type;
	self->lattice = hkl_lattice_new_copy(src->lattice);
	self->U = src->U;
	self->UB = src->UB;

	// copy the reflections
	len = HKL_LIST_LEN(src->reflections);
	HKL_LIST_ALLOC(self->reflections, len);
	for(i=0; i<len; ++i)
		self->reflections[i] = hkl_sample_reflection_new_copy(src->reflections[i]);

	return self;
}

void hkl_sample_free(HklSample *self)
{
	if (!self)
		return;

	free(self->name);
	hkl_lattice_free(self->lattice);
	HKL_LIST_FREE_DESTRUCTOR(self->reflections, hkl_sample_reflection_free);
	free(self);
}

void hkl_sample_set_name(HklSample *self, char const *name)
{
	if (!self)
		return;

	if(self->name)
		free(self->name);
	self->name = strdup(name);
}

int hkl_sample_set_lattice(HklSample *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma)
{
	int status;

	if (!self)
		return HKL_FAIL;


	status = hkl_lattice_set(self->lattice, a, b, c, alpha, beta, gamma);
	if (status == HKL_SUCCESS)
		hkl_sample_compute_UB(self);
	return status;
}

/* TODO test */
int hkl_sample_set_U_from_euler(HklSample *self,
				double x, double y, double z)
{
	if (!self)
		return HKL_FAIL;

	hkl_matrix_from_euler(&self->U, x, y, z);
	hkl_sample_compute_UB(self);

	return HKL_SUCCESS;
}

void hkl_sample_get_UB(HklSample *self, HklMatrix *UB)
{
	if (!self || !UB)
		return;

	hkl_sample_compute_UB(self);
	*UB = self->UB;
}

HklSampleReflection *hkl_sample_add_reflection(HklSample *self,
					       HklGeometry *geometry,
					       HklDetector const *detector,
					       double h, double k, double l)
{
	HklSampleReflection *ref;

	ref = hkl_sample_reflection_new(geometry, detector, h, k, l);

	if(ref)
		HKL_LIST_ADD_VALUE(self->reflections, ref);

	return ref;
}

HklSampleReflection* hkl_sample_get_ith_reflection(HklSample const *self, size_t idx)
{
	if (!self)
		return NULL;

	return self->reflections[idx];
}

int hkl_sample_del_reflection(HklSample *self, size_t idx)
{
	if (!self)
		return HKL_FAIL;

	hkl_sample_reflection_free(self->reflections[idx]);
	HKL_LIST_DEL(self->reflections, idx);

	return HKL_SUCCESS;
}

int hkl_sample_compute_UB_busing_levy(HklSample *self, size_t idx1, size_t idx2)
{
	HklSampleReflection *r1;
	HklSampleReflection *r2;

	if (!self
	    || idx1 >= HKL_LIST_LEN(self->reflections)
	    || idx2 >= HKL_LIST_LEN(self->reflections))
		return HKL_FAIL;

	r1 = self->reflections[idx1];
	r2 = self->reflections[idx2];

	if (!hkl_vector_is_colinear(&r1->hkl, &r2->hkl)) {
		HklVector h1c;
		HklVector h2c;
		HklMatrix B;
		HklMatrix Tc;

		// Compute matrix Tc from r1 and r2.
		h1c = r1->hkl;
		h2c = r2->hkl;
		hkl_lattice_get_B(self->lattice, &B);
		hkl_matrix_times_vector(&B, &h1c);
		hkl_matrix_times_vector(&B, &h2c);
		hkl_matrix_from_two_vector(&Tc, &h1c, &h2c);
		hkl_matrix_transpose(&Tc);

		// compute U
		hkl_matrix_from_two_vector(&self->U,
					   &r1->_hkl, &r2->_hkl);
		hkl_matrix_times_smatrix(&self->U, &Tc);
		hkl_sample_compute_UB(self);
	} else
		return HKL_FAIL;
	
	return HKL_SUCCESS;
}

double hkl_sample_affine(HklSample *self)
{
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size;

	if (!self)
		return GSL_NAN;

	// Starting point
	x = gsl_vector_alloc (9);
	gsl_vector_set (x, 0, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 1, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 2, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 3, self->lattice->a->value);
	gsl_vector_set (x, 4, self->lattice->b->value);
	gsl_vector_set (x, 5, self->lattice->c->value);
	gsl_vector_set (x, 6, self->lattice->alpha->value);
	gsl_vector_set (x, 7, self->lattice->beta->value);
	gsl_vector_set (x, 8, self->lattice->gamma->value);

	// Set initial step sizes to 1
	ss = gsl_vector_alloc (9);
	gsl_vector_set (ss, 0, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 1, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 2, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 3, !self->lattice->a->not_to_fit);
	gsl_vector_set (ss, 4, !self->lattice->b->not_to_fit);
	gsl_vector_set (ss, 5, !self->lattice->c->not_to_fit);
	gsl_vector_set (ss, 6, !self->lattice->alpha->not_to_fit);
	gsl_vector_set (ss, 7, !self->lattice->beta->not_to_fit);
	gsl_vector_set (ss, 8, !self->lattice->gamma->not_to_fit);

	// Initialize method and iterate
	minex_func.n = 9;
	minex_func.f = &mono_crystal_fitness;
	minex_func.params = self;
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

double hkl_sample_get_reflection_mesured_angle(HklSample const *self,
					       size_t idx1, size_t idx2)
{
	if (!self
	    || idx1 >= HKL_LIST_LEN(self->reflections)
	    || idx2 >= HKL_LIST_LEN(self->reflections))
		return GSL_NAN;

	return hkl_vector_angle(&self->reflections[idx1]->_hkl,
				&self->reflections[idx2]->_hkl);
}

double hkl_sample_get_reflection_theoretical_angle(HklSample const *self,
						   size_t idx1, size_t idx2)
{
	if (!self
	    || idx1 >= HKL_LIST_LEN(self->reflections)
	    || idx2 >= HKL_LIST_LEN(self->reflections))
		return GSL_NAN;

	HklVector hkl1;
	HklVector hkl2;

	hkl1 = self->reflections[idx1]->hkl;
	hkl2 = self->reflections[idx2]->hkl;
	hkl_matrix_times_vector(&self->UB, &hkl1);
	hkl_matrix_times_vector(&self->UB, &hkl2);

	return hkl_vector_angle(&hkl1, &hkl2);
}

void hkl_sample_fprintf(FILE *f,  HklSample const *self)
{
	size_t i, len;

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
	fprintf(f, "\nUB:\n");
	hkl_matrix_fprintf(f, &self->UB);

	len = HKL_LIST_LEN(self->reflections);
	if (len){
		HklSampleReflection *reflection;
		HklAxis *axes;
		size_t axes_len;

		reflection  = hkl_sample_get_ith_reflection(self, 0);

		fprintf(f, "Reflections:");
		fprintf(f, "\n");
		fprintf(f, "i %-10.6s %-10.6s %-10.6s", "h", "k", "l");
		axes = reflection->geometry->axes;
		axes_len = HKL_LIST_LEN(reflection->geometry->axes);
		for(i=0; i<axes_len; ++i)
			fprintf(f, " %-10.6s", hkl_axis_get_name(&axes[i]));

		for(i=0; i<len; ++i){
			size_t j;
			
			reflection  = hkl_sample_get_ith_reflection(self, i);
			axes = reflection->geometry->axes;
			axes_len = HKL_LIST_LEN(reflection->geometry->axes);
			fprintf(f, "\n%d %-10.6f %-10.6f %-10.6f", i, 
				reflection->hkl.data[0], reflection->hkl.data[1], reflection->hkl.data[2]);
			for(j=0; j<axes_len; ++j)
				fprintf(f, " %-10.6f", hkl_axis_get_value_unit(&axes[j]));
		}
	}
}

/***********************/
/* HklSampleReflection */
/***********************/

void hkl_sample_reflection_set_hkl(HklSampleReflection *self, double h, double k, double l)
{
	if(!self
	   || (fabs(h) + fabs(k) + fabs(l) < HKL_EPSILON))
		return;

	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
}

void hkl_sample_reflection_set_flag(HklSampleReflection *self, int flag)
{
	if(!self)
		return;
	self->flag = flag;
}

/*****************/
/* HklSampleList */
/*****************/

HklSampleList *hkl_sample_list_new(void)
{
	HklSampleList *self = NULL;
	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate memory for an HklSampleList");

	HKL_LIST_INIT(self->samples);
	self->current = NULL;

	return self;
}

void hkl_sample_list_free(HklSampleList *self)
{
	if (self){
		HKL_LIST_FREE_DESTRUCTOR(self->samples, hkl_sample_free);
		self->current = NULL;
		free(self);
	}
}

void hkl_sample_list_clear(HklSampleList *self)
{
	if(self){
		HKL_LIST_FREE_DESTRUCTOR(self->samples, hkl_sample_free);
		self->current = NULL;
		HKL_LIST_INIT(self->samples);
	}
}

HklSample *hkl_sample_list_append(HklSampleList *self, HklSample *sample)
{
	if (!self || !sample
	    || hkl_sample_list_get_idx_from_name(self, sample->name) != HKL_FAIL)
		return NULL;

	HKL_LIST_ADD_VALUE(self->samples, sample);

	return sample;
}

void hkl_sample_list_del(HklSampleList *self, HklSample *sample)
{
	if(self && sample){
		if (self->current == sample)
			self->current = NULL;
		HKL_LIST_DEL_ITEM_DESTRUCTOR(self->samples, sample, hkl_sample_free);
	}
}

/* TODO test */
size_t hkl_sample_list_len(HklSampleList const *self)
{
	return HKL_LIST_LEN(self->samples);
}

/* TODO test */
HklSample *hkl_sample_list_get_ith(HklSampleList *self, size_t idx)
{
	HklSample *sample = NULL;
	if (self && idx < HKL_LIST_LEN(self->samples))
		sample = self->samples[idx];

	return sample;
}

/* TODO test */
HklSample *hkl_sample_list_get_by_name(HklSampleList *self, char const *name)
{
	HklSample *sample = NULL;
	size_t idx;

	if (!self || !name)
		return sample;

	idx = hkl_sample_list_get_idx_from_name(self, name);
	if (HKL_FAIL != idx)
		sample = self->samples[idx];

	return sample;
}

size_t hkl_sample_list_get_idx_from_name(HklSampleList *self, char const *name)
{
	size_t idx;

	if (!self || !name || !self->samples)
		return HKL_FAIL;

	for(idx=0; idx<HKL_LIST_LEN(self->samples); ++idx)
		if (!strcmp(self->samples[idx]->name, name))
			return idx;

	return HKL_FAIL;
}

int hkl_sample_list_select_current(HklSampleList *self, char const *name)
{
	size_t idx;
	int res = HKL_FAIL;

	if(!self || !name || !self->samples)
		return res;

	idx = hkl_sample_list_get_idx_from_name(self, name);
	if (idx != HKL_FAIL){
		self->current = self->samples[idx];
		res = HKL_SUCCESS;
	}

	return res;
}

void hkl_sample_list_fprintf(FILE *f, HklSampleList const *self)
{
	size_t i;
	for(i=0; i<HKL_LIST_LEN(self->samples); ++i)
		hkl_sample_fprintf(f, self->samples[i]);
}
