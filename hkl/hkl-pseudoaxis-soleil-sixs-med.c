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
 * Copyright (C) 2011 Synchrotron SOLEIL
 *                    L'Orme des Merisiers Saint-Aubin
 *                    BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-pseudoaxis-soleil-sixs-med.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>
#include <hkl/hkl-vector.h>

/**************************/
/* MED2+2 PseudoAxeEngine */
/**************************/

static int reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	double mu, gamma;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	gamma = x_data[2];

	f_data[3] = gamma - 2 * mu;

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_soleil_sixs_med_2_2_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* mu_fixed" */
	mode = hkl_pseudo_axis_engine_mode_new(
		"mu_fixed",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* reflectivity */
	mode = hkl_pseudo_axis_engine_mode_new(
		"reflectivity",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, reflectivity,
		(size_t)0,
		(size_t)4, "mu", "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);


	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}

/**************************/
/* MED1+2 PseudoAxeEngine */
/**************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_soleil_sixs_med_1_2_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* pitch_fixed" */
	mode = hkl_pseudo_axis_engine_mode_new(
		"pitch_fixed",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "mu", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* delta_fixed" */
	mode = hkl_pseudo_axis_engine_mode_new(
		"delta_fixed",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "pitch", "mu", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}

/**************************/
/* MED2+3 PseudoAxeEngine */
/**************************/

typedef struct _HklSlitsFit HklSlitsFit;
struct _HklSlitsFit
{
	HklGeometry *geometry;
	HklVector surface;
	unsigned int slits_id;
	unsigned int len;
	HklAxis *axis;
};

static int slits_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklVector n_slits = {{0, 0, 1}};
	HklSlitsFit *parameters = params;

	hkl_axis_set_value(parameters->axis, x_data[0]);
	hkl_geometry_update(parameters->geometry);

	/* compute the orientation of the slits */
	hkl_vector_rotated_quaternion(&n_slits,
				      &parameters->geometry->holders[1].q);

	/* both directions must be perpendicular */
	f_data[0] = hkl_vector_scalar_product(&parameters->surface, &n_slits);

	return  GSL_SUCCESS;
}

static int fit_slits_orientation(HklSlitsFit *params)
{
	size_t i;
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_multiroot_function f;
	gsl_vector *x;
	double *x_data;
	int status;
	int res = HKL_FAIL;
	int iter;

	/* now solve the system */
	/* Initialize method  */
	T = gsl_multiroot_fsolver_hybrid;
	s = gsl_multiroot_fsolver_alloc (T, params->len);
	x = gsl_vector_alloc(params->len);
	x_data = gsl_vector_ptr(x, 0);

	/* initialize x with the right values */
	x_data[0] = hkl_axis_get_value(params->axis);

	f.f = slits_func;
	f.n = params->len;
	f.params = params;
	gsl_multiroot_fsolver_set (s, &f, x);

	/* iterate to find the solution */
	iter = 0;
	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		if (status || iter % 100 == 0) {
			/* Restart from another point. */
			for(i=0; i<params->len; ++i)
				x_data[i] = (double)rand() / RAND_MAX * 180. / M_PI;
			gsl_multiroot_fsolver_set(s, &f, x);
			gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON);
	} while (status == GSL_CONTINUE && iter < 1000);

#ifdef DEBUG
	fprintf(stdout, "\n  fitting the detector position using thoses axes :");
	for(i=0; i<params.len; ++i)
		fprintf(stdout, " \"%s\"", ((HklParameter *)params.axes[i])->name);
	fprintf(stdout, " status : %d iter : %d", status, iter);
	fprintf(stdout, " x: [");
	for(i=0; i<params.len; ++i)
		fprintf(stdout, " %.7f", s->x->data[i]);
	fprintf(stdout, "] f: [");
	for(i=0; i<params.len; ++i)
		fprintf(stdout, " %.7f", s->f->data[i]);
	fprintf(stdout, "]\n");
	hkl_geometry_fprintf(stdout, params.geometry);
#endif
	if(status != GSL_CONTINUE){
		res = HKL_SUCCESS;
		/* put the axes in the -pi, pi range. */
		gsl_sf_angle_restrict_pos_e(&((HklParameter *)params->axis)->value);
	}
	/* release memory */
	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return res;
}

void hkl_geometry_list_multiply_soleil_sixs_med_2_3(HklGeometryList *self, size_t idx)
{
	unsigned int i;
	unsigned int len;
	HklSlitsFit params;
	HklGeometry *geometry;
	double slits_position;

	/* For each solution already found we will generate another one */
	/* we will set the right slit orientation for a given detector arm position */
	geometry = self->items[idx]->geometry;

	/* get the index of the axis corresponding to the slits */
	/* for now the last holder is the detector one */
	params.slits_id = geometry->holders[1].idx[HKL_LIST_LEN(geometry->holders[1].idx)-1];
	params.len = 1; /* only one axis to fit */
	params.geometry = geometry;
	params.axis = &params.geometry->axes[params.slits_id];

	/* compute the surface orientation fixed during the fit */
	/* use the last sample axis as sample surface normal */
	params.surface = geometry->axes[geometry->holders[0].idx[HKL_LIST_LEN(geometry->holders[0].idx) - 1]].axis_v;
	hkl_vector_rotated_quaternion(&params.surface,
				      &params.geometry->holders[0].q);


	/* we just need to fit the slits orientation */
	/* save it's value before */
	slits_position = hkl_axis_get_value(params.axis);
	if (fit_slits_orientation(&params) != HKL_SUCCESS)
		hkl_axis_set_value(params.axis, slits_position);
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_soleil_sixs_med_2_3_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* mu_fixed */
	mode = hkl_pseudo_axis_engine_mode_new(
		"mu_fixed",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* gamma_fixed */
	mode = hkl_pseudo_axis_engine_mode_new(
		"gamma_fixed",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "mu", "omega", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);


	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
