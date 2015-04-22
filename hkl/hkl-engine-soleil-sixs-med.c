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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_multiroots.h>
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-axis-private.h"           // for HklAxis
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_engine_hkl_new, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for hkl_engine_tth2_new, etc
#include "hkl-pseudoaxis-common-readonly-private.h"

#define PITCH "pitch"
#define BETA "beta"
#define MU "mu"
#define OMEGA "omega"

/* #define DEBUG */

/*********************/
/* MED 2+2 HklEngine */
/*********************/

static int _reflectivity_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[2];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = gamma - 2 * mu;

	return  GSL_SUCCESS;
}

static const HklFunction reflectivity_func = {
	.function = _reflectivity_func,
	.size = 4,
};

static HklMode* mu_fixed_2_2()
{
	static const char* axes_r[] = {"beta", "mu", "omega", "gamma", "delta"};
	static const char* axes_w[] = {"omega", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("mu_fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode* reflectivity_2_2()
{
	static const char* axes_r[] = {"beta", "mu", "omega", "gamma", "delta"};
	static const char* axes_w[] = {"mu", "omega", "gamma", "delta"};
	static const HklFunction *functions[] = {&reflectivity_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("reflectivity", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_soleil_sixs_med_2_2_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = mu_fixed_2_2();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, reflectivity_2_2());

	return self;
}

/* mode incidence */

static const char *soleil_sixs_med_2_2_incidence_axes[] = {BETA, MU, OMEGA};

REGISTER_INCIDENCE_ENGINE(soleil_sixs_med_2_2);

/*********************/
/* MED 1+2 HklEngine */
/*********************/

static HklMode* pitch_fixed()
{
	static const char *axes_r[] = {"pitch", "mu", "gamma", "delta"};
	static const char* axes_w[] = {"mu", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_soleil_sixs_med_1_2_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = pitch_fixed();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/* mode incidence */

static const char *soleil_sixs_med_1_2_incidence_axes[] = {PITCH, MU};

REGISTER_INCIDENCE_ENGINE(soleil_sixs_med_1_2);

/*********************/
/* MED 2+3 HklEngine */
/*********************/

typedef struct _HklSlitsFit HklSlitsFit;
struct _HklSlitsFit
{
	HklGeometry *geometry;
	HklVector surface;
	unsigned int slits_id;
	unsigned int len;
	HklParameter *axis;
};

static int slits_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklVector n_slits = {{0, 0, 1}};
	HklSlitsFit *parameters = params;

	hkl_parameter_value_set(parameters->axis, x_data[0], HKL_UNIT_DEFAULT, NULL);
	hkl_geometry_update(parameters->geometry);

	/* compute the orientation of the slits */
	hkl_vector_rotated_quaternion(&n_slits,
				      &darray_item(parameters->geometry->holders, 1)->q);

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
	int res = FALSE;
	int iter;

	/* now solve the system */
	/* Initialize method  */
	T = gsl_multiroot_fsolver_hybrid;
	s = gsl_multiroot_fsolver_alloc (T, params->len);
	x = gsl_vector_alloc(params->len);
	x_data = gsl_vector_ptr(x, 0);

	/* initialize x with the right values */
	x_data[0] = params->axis->_value;

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
	for(i=0; i<params->len; ++i)
		fprintf(stdout, " \"%s\"", params->axis->name);
	fprintf(stdout, " status : %d iter : %d", status, iter);
	fprintf(stdout, " x: [");
	for(i=0; i<params->len; ++i)
		fprintf(stdout, " %.7f", s->x->data[i]);
	fprintf(stdout, "] f: [");
	for(i=0; i<params->len; ++i)
		fprintf(stdout, " %.7f", s->f->data[i]);
	fprintf(stdout, "]\n");
	hkl_geometry_fprintf(stdout, params->geometry);
#endif
	if(status != GSL_CONTINUE){
		res = TRUE;
		/* put the axes in the -pi, pi range. */
		gsl_sf_angle_restrict_pos_e(&params->axis->_value);
	}
	/* release memory */
	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return res;
}

static void hkl_geometry_list_multiply_soleil_sixs_med_2_3(HklGeometryList *self,
							   HklGeometryListItem *item)
{
	unsigned int i;
	unsigned int len;
	HklSlitsFit params;
	HklGeometry *geometry;
	double slits_position;
	HklHolder *sample_holder;
	HklHolder *detector_holder;

	/* For each solution already found we will generate another one */
	/* we will set the right slit orientation for a given detector arm position */
	geometry = item->geometry;
	sample_holder = darray_item(geometry->holders, 0);
	detector_holder = darray_item(geometry->holders, 1);

	/* get the index of the axis corresponding to the slits */
	/* for now the last holder is the detector one */
	params.slits_id = detector_holder->config->idx[detector_holder->config->len-1];
	params.len = 1; /* only one axis to fit */
	params.geometry = geometry;
	params.axis = darray_item(params.geometry->axes, params.slits_id);

	/* compute the surface orientation fixed during the fit */
	/* use the last sample axis as sample surface normal */
	params.surface = container_of(darray_item(geometry->axes,
						  sample_holder->config->idx[sample_holder->config->len - 1]),
				      HklAxis, parameter)->axis_v;
	hkl_vector_rotated_quaternion(&params.surface,
				      &sample_holder->q);


	/* we just need to fit the slits orientation */
	/* save it's value before */
	slits_position = hkl_parameter_value_get(params.axis, HKL_UNIT_DEFAULT);
	if (fit_slits_orientation(&params) != TRUE)
		hkl_parameter_value_set(params.axis, slits_position, HKL_UNIT_DEFAULT, NULL);
}

static HklMode* mu_fixed_2_3()
{
	static const char *axes_r[] = {"beta", "mu", "omega", "gamma", "delta", "eta_a"};
	static const char* axes_w[] = {"omega", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("mu_fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_soleil_sixs_med_2_3_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = mu_fixed_2_3();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/***********************/
/* SOLEIL SIXS MED 2+2 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_soleil_sixs_med_2_2_axes[] = {"beta", "mu", "omega", "gamma", "delta"};

static HklGeometry *hkl_geometry_new_soleil_sixs_med_2_2(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_2_2(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_soleil_sixs_med_2_2_hkl_new(self);
	hkl_engine_q2_new(self);
	hkl_engine_qper_qpar_new(self);
	hkl_engine_tth2_new(self);
	hkl_engine_soleil_sixs_med_2_2_incidence_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_2,"SOLEIL SIXS MED2+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION);

/***********************/
/* SOLEIL SIXS MED 1+2 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_soleil_sixs_med_1_2_axes[] = {"pitch", "mu", "gamma", "delta"};

static HklGeometry *hkl_geometry_new_soleil_sixs_med_1_2(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_1_2(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_soleil_sixs_med_1_2_hkl_new(self);
	hkl_engine_q2_new(self);
	hkl_engine_qper_qpar_new(self);
	hkl_engine_tth2_new(self);
	hkl_engine_soleil_sixs_med_1_2_incidence_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_1_2, "SOLEIL SIXS MED1+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION);


/***********************/
/* SOLEIL SIXS MED 2+3 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 4 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **eta_a** : rotation around the :math:`-\\vec{x}` direction (-1, 0, 0)\n"

static const char* hkl_geometry_soleil_sixs_med_2_3_axes[] = {"beta", "mu", "omega", "gamma", "delta", "eta_a"};

static HklGeometry *hkl_geometry_new_soleil_sixs_med_2_3(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "eta_a", -1, 0, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_2_3(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	self->geometries->multiply = hkl_geometry_list_multiply_soleil_sixs_med_2_3;
	hkl_engine_soleil_sixs_med_2_3_hkl_new(self);
	hkl_engine_q2_new(self);
	hkl_engine_qper_qpar_new(self);
	hkl_engine_tth2_new(self);
	hkl_engine_soleil_sixs_med_2_2_incidence_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_3, "SOLEIL SIXS MED2+3", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION);
