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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_sf_trig.h>

#include <string.h>
#include <ccan/array_size/array_size.h>
#include <gsl/gsl_sf_trig.h>

#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-common-q-private.h"

/*****/
/* q */
/*****/

struct _HklPseudoAxisEngineQ
{
	HklPseudoAxisEngine engine;
	HklParameter *q;
};

static int _q_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	double q;
	HklPseudoAxisEngine *engine = params;
	const HklPseudoAxisEngineQ *engine_q = container_of(engine, HklPseudoAxisEngineQ, engine);
	double tth;

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x */
	set_geometry_axes(engine, x->data);

	tth = gsl_sf_angle_restrict_symm(x->data[0]);
	q = 2 * HKL_TAU / hkl_source_get_wavelength(&engine->geometry->source) * sin(tth/2.);

	f->data[0] = engine_q->q->_value - q;

	return  GSL_SUCCESS;
}

static const HklFunction q_func = {
	.function = _q_func,
	.size = 1,
};

static int get_q_real(HklPseudoAxisEngineMode *self,
		      HklPseudoAxisEngine *base,
		      HklGeometry *geometry,
		      HklDetector *detector,
		      HklSample *sample,
		      HklError **error)
{
	double wavelength;
	double theta;
	HklVector ki, kf;
	HklPseudoAxisEngineQ *engine = container_of(base, HklPseudoAxisEngineQ, engine);

	wavelength = hkl_source_get_wavelength(&geometry->source);
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;

	/* we decide of the sign of theta depending on the orientation
	 * of kf in the direct-space */
	if(kf.data[1] < 0 || kf.data[2] < 0)
		theta = -theta;

	/* update q */
	engine->q->_value = 2 * HKL_TAU / wavelength * sin(theta);

	return HKL_TRUE;
}

/* not declared in the constructor as it is used also in the q2 pseudo
 * axis engine */
static const HklPseudoAxis q = {
	.parameter = {HKL_PARAMETER_DEFAULTS, .name="q", .range={.min=-1, .max=1}},
};

static HklPseudoAxisEngineMode *mode_q(void)
{
	static const char *axes[] = {"tth"};
	static const HklFunction *functions[] = {&q_func};
	static HklPseudoAxisEngineModeAutoInfo info = {
		INFO_AUTO("q", axes, functions),
	};
	static const HklPseudoAxisEngineModeOperations operations = {
		HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_q_real,
	};

	return hkl_pseudo_axis_engine_mode_auto_new(&info, &operations);
}

static void hkl_pseudo_axis_engine_q_free_real(HklPseudoAxisEngine *base)
{
	HklPseudoAxisEngineQ *self=container_of(base, HklPseudoAxisEngineQ, engine);
	hkl_pseudo_axis_engine_release(&self->engine);
	free(self);
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_q_new(void)
{
	HklPseudoAxisEngineQ *self;
	HklPseudoAxisEngineMode *mode;
	static const HklPseudoAxis *pseudo_axes[] = {&q};
	static const HklPseudoAxisEngineInfo info = {
		.name = "q",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};
	static const HklPseudoAxisEngineOperations operations = {
		HKL_PSEUDO_AXIS_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_pseudo_axis_engine_q_free_real,
	};

	self = HKL_MALLOC(HklPseudoAxisEngineQ);

	hkl_pseudo_axis_engine_init(&self->engine, &info, &operations);
	self->q = register_pseudo_axis(&self->engine, &q.parameter);

	/* q [default] */
	mode = mode_q();
	hkl_pseudo_axis_engine_add_mode(&self->engine, mode);
	hkl_pseudo_axis_engine_select_mode(&self->engine, mode);

	return &self->engine;
}

/******/
/* q2 */
/******/

struct _HklPseudoAxisEngineQ2
{
	HklPseudoAxisEngine engine;
	HklParameter *q;
	HklParameter *alpha;
};

static int _q2_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklPseudoAxisEngine *engine = params;
	const HklPseudoAxisEngineQ2 *engine_q2 = container_of(engine, HklPseudoAxisEngineQ2, engine);
	double q;
	double alpha;
	double wavelength, theta;
	HklVector kf, ki;
	HklVector X = {{1, 0, 0}};

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x */
	set_geometry_axes(engine, x->data);

	wavelength = hkl_source_get_wavelength(&engine->geometry->source);
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;

	q = 2 * HKL_TAU / wavelength * sin(theta);

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &X);
	alpha = atan2(kf.data[2], kf.data[1]);

	f->data[0] = engine_q2->q->_value - q;
	f->data[1] = engine_q2->alpha->_value - alpha;

	return  GSL_SUCCESS;
}

static const HklFunction q2_func = {
	.function = _q2_func,
	.size = 2,
};

static int get_q2_real(HklPseudoAxisEngineMode *self,
		       HklPseudoAxisEngine *engine,
		       HklGeometry *geometry,
		       HklDetector *detector,
		       HklSample *sample,
		       HklError **error)
{
	HklPseudoAxisEngineQ2 *engine_q2 = container_of(engine, HklPseudoAxisEngineQ2, engine);
	double wavelength;
	double theta;
	HklVector x = {{1, 0, 0}};
	HklVector ki, kf;

	wavelength = hkl_source_get_wavelength(&geometry->source);
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &x);

	/* update q and alpha */
	engine_q2->q->_value = 2 * HKL_TAU / wavelength * sin(theta);
	engine_q2->alpha->_value = atan2(kf.data[2], kf.data[1]);

	return HKL_TRUE;
}

static HklPseudoAxisEngineMode *mode_q2(void)
{
	static const char* axes[] = {"gamma", "delta"};
	static const HklFunction *functions[] = {&q2_func};
	static const HklPseudoAxisEngineModeAutoInfo info = {
		INFO_AUTO("q2", axes, functions),
	};
	static const HklPseudoAxisEngineModeOperations operations = {
		HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_q2_real,
	};

	return hkl_pseudo_axis_engine_mode_auto_new(&info, &operations);
}

static const HklPseudoAxis alpha = {
	.parameter = {HKL_PARAMETER_DEFAULTS_ANGLE, .name="alpha"},
};

static void hkl_pseudo_axis_engine_q2_free_real(HklPseudoAxisEngine *base)
{
	HklPseudoAxisEngineQ2 *self = container_of(base, HklPseudoAxisEngineQ2, engine);
	hkl_pseudo_axis_engine_release(&self->engine);
	free(self);
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_q2_new(void)
{
	HklPseudoAxisEngineQ2 *self;
	HklPseudoAxisEngineMode *mode;
	static const HklPseudoAxis *pseudo_axes[] = {&q, &alpha};
	static const HklPseudoAxisEngineInfo info = {
		.name = "q2",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};
	static const HklPseudoAxisEngineOperations operations = {
		HKL_PSEUDO_AXIS_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_pseudo_axis_engine_q2_free_real,
	};

	self = HKL_MALLOC(HklPseudoAxisEngineQ2);

	hkl_pseudo_axis_engine_init(&self->engine, &info, &operations);
	self->q = register_pseudo_axis(&self->engine, &q.parameter);
	self->alpha = register_pseudo_axis(&self->engine, &alpha.parameter);

	/* q2 [default] */
	mode = mode_q2();
	hkl_pseudo_axis_engine_add_mode(&self->engine, mode);
	hkl_pseudo_axis_engine_select_mode(&self->engine, mode);

	return &self->engine;
}
