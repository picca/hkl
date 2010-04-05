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
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <string.h>
#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-pseudoaxis-common.h>
#include <hkl/hkl-pseudoaxis-common-q.h>
#include <hkl/hkl-pseudoaxis-auto.h>
#include <gsl/gsl_sf_trig.h>

/*****/
/* q */
/*****/

static int q(const gsl_vector *x, void *params, gsl_vector *f)
{
	size_t i;
	double q;
	double q0;
	double tth;
	double const *x_data;
	double *f_data;
	HklPseudoAxisEngine *engine;

	x_data = gsl_vector_const_ptr(x, 0);
	f_data = gsl_vector_ptr(f, 0);
	engine = params;

	/* update the workspace from x */
	for(i=0; i<HKL_LIST_LEN(engine->axes); ++i)
		hkl_axis_set_value(engine->axes[i], x_data[i]);
	hkl_geometry_update(engine->geometry);

	q0 = ((HklParameter *)engine->pseudoAxes[0])->value;

	tth = gsl_sf_angle_restrict_symm(x_data[0]);
	q = 2 * HKL_TAU / hkl_source_get_wavelength(&engine->geometry->source) * sin(tth/2.);

	f_data[0] = q0 - q;

	return  GSL_SUCCESS;
}

static int hkl_pseudo_axis_engine_mode_get_q_real(HklPseudoAxisEngineMode *self,
						  HklPseudoAxisEngine *engine,
						  HklGeometry *geometry,
						  HklDetector *detector,
						  HklSample *sample,
						  HklError **error)
{
	double wavelength;
	double theta;
	double q;
	HklInterval range = {0};
	HklVector ki, kf;
	HklParameter *parameter;

	wavelength = hkl_source_get_wavelength(&geometry->source);
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;
	
	hkl_vector_vectorial_product(&ki, &kf);
	if(ki.data[1] > 0)
		theta = -theta;

	q = 2 * HKL_TAU / wavelength * sin(theta);

	/* update q */
	parameter = (HklParameter *)(engine->pseudoAxes[0]);
	hkl_parameter_set_value(parameter, q);
	hkl_parameter_set_range(parameter, range.min, range.max);

	return HKL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_q_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_new("q", 1, "q");

	/* q */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "q",
			   -1, 0., 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);
	/* q */
	mode = hkl_pseudo_axis_engine_mode_new(
		"q",
		NULL,
		hkl_pseudo_axis_engine_mode_get_q_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, q,
		(size_t)0,
		(size_t)1, "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);


	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}

/******/
/* q2 */
/******/

static int q2(const gsl_vector *x, void *params, gsl_vector *f)
{
	size_t i;
	double q0, q;
	double alpha0, alpha;
	double wavelength, theta;
	double const *x_data;
	double *f_data;
	HklPseudoAxisEngine *engine;
	HklVector kf, ki;
	HklVector X = {{1, 0, 0}};

	x_data = gsl_vector_const_ptr(x, 0);
	f_data = gsl_vector_ptr(f, 0);
	engine = params;

	/* update the workspace from x */
	for(i=0; i<HKL_LIST_LEN(engine->axes); ++i)
		hkl_axis_set_value(engine->axes[i], x_data[i]);
	hkl_geometry_update(engine->geometry);

	q0 = ((HklParameter *)engine->pseudoAxes[0])->value;
	alpha0 = ((HklParameter *)engine->pseudoAxes[1])->value;

	wavelength = hkl_source_get_wavelength(&engine->geometry->source);
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;
	
	q = 2 * HKL_TAU / wavelength * sin(theta);

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &X);
	alpha = atan2(kf.data[2], kf.data[1]);

	f_data[0] = q0 - q;
	f_data[1] = alpha0 - alpha;

	return  GSL_SUCCESS;
}


static int hkl_pseudo_axis_engine_mode_get_q2_real(HklPseudoAxisEngineMode *self,
						   HklPseudoAxisEngine *engine,
						   HklGeometry *geometry,
						   HklDetector *detector,
						   HklSample *sample,
						   HklError **error)
{
	double wavelength;
	double theta;
	double q, alpha;
	HklVector x = {{1, 0, 0}};
	HklVector ki, kf;
	HklParameter *parameter;

	wavelength = hkl_source_get_wavelength(&geometry->source);
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;
	
	q = 2 * HKL_TAU / wavelength * sin(theta);

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &x);
	alpha = atan2(kf.data[2], kf.data[1]);

	/* update q */
	parameter = (HklParameter *)(engine->pseudoAxes[0]);
	hkl_parameter_set_value(parameter, q);

	/* update alpha */
	parameter = (HklParameter *)(engine->pseudoAxes[1]);
	hkl_parameter_set_value(parameter, alpha);
	hkl_parameter_set_range(parameter, -M_PI, M_PI);

	return HKL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_q2_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_new("q2", 2, "q", "alpha");

	/* q */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "q",
			   0., 0., 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	/* alpha */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[1],
			   "alpha",
			   -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	/* q */
	mode = hkl_pseudo_axis_engine_mode_new(
		"q2",
		NULL,
		hkl_pseudo_axis_engine_mode_get_q2_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, q2,
		(size_t)0,
		(size_t)2, "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);


	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
