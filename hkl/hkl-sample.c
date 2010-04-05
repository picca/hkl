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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
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
	hkl_vector_rotated_quaternion(&self->_hkl, &self->geometry->holders[self->detector.idx].q);
	hkl_vector_minus_vector(&self->_hkl, &ki);

	q = self->geometry->holders[0].q;
	hkl_quaternion_conjugate(&q);
	hkl_vector_rotated_quaternion(&self->_hkl, &q);
}

static HklSampleReflection *hkl_sample_reflection_new(HklGeometry *geometry,
						      HklDetector const *detector,
						      double h, double k, double l)
{
	HklSampleReflection *self;

	if (!geometry || !detector)
		return NULL;

	self = HKL_MALLOC(HklSampleReflection);

	hkl_geometry_update(geometry);

	self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = *detector;
	self->hkl.data[0] = h;
	self->hkl.data[1] = k;
	self->hkl.data[2] = l;
	self->flag = HKL_TRUE;

	hkl_sample_reflection_update(self);

	return self;
}

static HklSampleReflection *hkl_sample_reflection_new_copy(HklSampleReflection const *src)
{
	HklSampleReflection *self = NULL;

	self = HKL_MALLOC(HklSampleReflection);

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

static void hkl_sample_compute_UxUyUz(HklSample *self)
{
	double ux;
	double uy;
	double uz;

	hkl_matrix_to_euler(&self->U, &ux, &uy, &uz);
	hkl_parameter_set_value(self->ux, ux);
	hkl_parameter_set_value(self->uy, uy);
	hkl_parameter_set_value(self->uz, uz);
}

static int hkl_sample_compute_UB(HklSample *self)
{
	HklMatrix B;

	if (hkl_lattice_get_B(self->lattice, &B))
		return HKL_FAIL;

	self->UB = self->U;
	hkl_matrix_times_matrix(&self->UB, &B);

	return HKL_SUCCESS;
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

static double set_UB_fitness(gsl_vector const *x, void *params)
{
	size_t i, j;
	double fitness;
	double euler_x;
	double euler_y;
	double euler_z;
	struct set_UB_t *parameters = params;
	HklSample *sample = parameters->sample;
	const HklMatrix *UB = parameters->UB;

	sample->ux->value = euler_x = gsl_vector_get(x, 0);
	sample->uy->value = euler_y = gsl_vector_get(x, 1);
	sample->uz->value = euler_z = gsl_vector_get(x, 2);
	sample->lattice->a->value = gsl_vector_get(x, 3);
	sample->lattice->b->value = gsl_vector_get(x, 4);
	sample->lattice->c->value = gsl_vector_get(x, 5);
	sample->lattice->alpha->value = gsl_vector_get(x, 6);
	sample->lattice->beta->value = gsl_vector_get(x, 7);
	sample->lattice->gamma->value = gsl_vector_get(x, 8);
	hkl_matrix_init_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<3; ++i)
		for(j=0; j<3; ++j){
			double tmp = UB->data[i][j] - sample->UB.data[i][j];
			fitness += tmp * tmp;
		}
	return fitness;
}

static double mono_crystal_fitness(gsl_vector const *x, void *params)
{
	size_t i, j;
	double fitness;
	double euler_x;
	double euler_y;
	double euler_z;
	HklSample *sample = params;

	sample->ux->value = euler_x = gsl_vector_get(x, 0);
	sample->uy->value = euler_y = gsl_vector_get(x, 1);
	sample->uz->value = euler_z = gsl_vector_get(x, 2);
	sample->lattice->a->value = gsl_vector_get(x, 3);
	sample->lattice->b->value = gsl_vector_get(x, 4);
	sample->lattice->c->value = gsl_vector_get(x, 5);
	sample->lattice->alpha->value = gsl_vector_get(x, 6);
	sample->lattice->beta->value = gsl_vector_get(x, 7);
	sample->lattice->gamma->value = gsl_vector_get(x, 8);
	hkl_matrix_init_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i) {
		HklSampleReflection *reflection;

		reflection = sample->reflections[i];
		if(reflection->flag == HKL_TRUE){
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
	gsl_vector_set (x, 0, sample->ux->value);
	gsl_vector_set (x, 1, sample->uy->value);
	gsl_vector_set (x, 2, sample->uz->value);
	gsl_vector_set (x, 3, sample->lattice->a->value);
	gsl_vector_set (x, 4, sample->lattice->b->value);
	gsl_vector_set (x, 5, sample->lattice->c->value);
	gsl_vector_set (x, 6, sample->lattice->alpha->value);
	gsl_vector_set (x, 7, sample->lattice->beta->value);
	gsl_vector_set (x, 8, sample->lattice->gamma->value);

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

HklSample* hkl_sample_new(char const *name, HklSampleType type)
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
	HKL_LIST_INIT(self->reflections);

	return self;
}

HklSample *hkl_sample_new_copy(HklSample const *src)
{
	HklSample *self = NULL;
	size_t len;
	size_t i;

	/* check parameters */
	if(!src)
		return self;

	self = HKL_MALLOC(HklSample);

	self->name = strdup(src->name);
	self->type = src->type;
	self->lattice = hkl_lattice_new_copy(src->lattice);
	self->U = src->U;
	self->UB = src->UB;
	self->ux = hkl_parameter_new_copy(src->ux);
	self->uy = hkl_parameter_new_copy(src->uy);
	self->uz = hkl_parameter_new_copy(src->uz);

	/* copy the reflections */
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
	hkl_parameter_free(self->ux);
	hkl_parameter_free(self->uy);
	hkl_parameter_free(self->uz);
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

	hkl_matrix_init_from_euler(&self->U, x, y, z);
	hkl_sample_compute_UB(self);
	hkl_parameter_set_value(self->ux, x);
	hkl_parameter_set_value(self->uy, y);
	hkl_parameter_set_value(self->uz, z);

	return HKL_SUCCESS;
}

void hkl_sample_get_UB(HklSample *self, HklMatrix *UB)
{
	if (!self || !UB)
		return;

	hkl_sample_compute_UB(self);
	*UB = self->UB;
}

/**
 * hkl_sample_set_UB: set the UB matrix of the sample
 * @self: the sample to modify
 * @UB: the UB matrix to set
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
		return;

	params.sample = self;
	params.UB = UB;

	return minimize(self, set_UB_fitness, &params);
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
		return HKL_FAIL;
	
	return HKL_SUCCESS;
}

double hkl_sample_affine(HklSample *self)
{
	if(!self)
		return GSL_NAN;

	return minimize(self, mono_crystal_fitness, self);
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
	HklVector hkl1;
	HklVector hkl2;

	if (!self
	    || idx1 >= HKL_LIST_LEN(self->reflections)
	    || idx2 >= HKL_LIST_LEN(self->reflections))
		return GSL_NAN;

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
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->ux);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->uy);
	fprintf(f, "\n ");
	hkl_parameter_fprintf(f, self->uz);
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

HklSampleList *hkl_sample_list_new(void)
{
	HklSampleList *self = NULL;
	self = HKL_MALLOC(HklSampleList);

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
