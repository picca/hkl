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

static int get_q_real(HklMode *self,
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
	.parameter = {HKL_PARAMETER_DEFAULTS, .name="q", .range={.max=1}},
};

static HklMode *mode_q(void)
{
	static const char *axes[] = {"tth"};
	static const HklFunction *functions[] = {&q_func};
	static HklModeAutoInfo info = {
		INFO_AUTO("q", axes, functions),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_q_real,
	};

	return hkl_mode_auto_new(&info, &operations);
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
	HklMode *mode;
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

static void _q2(HklGeometry *geometry, HklDetector *detector,
		double *q, double *alpha)
{
	double wavelength, theta;
	HklVector kf, ki;
	static HklVector x = {
		.data = {1, 0, 0},
	};

	wavelength = hkl_source_get_wavelength(&geometry->source);
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	theta = hkl_vector_angle(&ki, &kf) / 2.;

	*q = 2 * HKL_TAU / wavelength * sin(theta);

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &x);

	*alpha = atan2(kf.data[2], kf.data[1]);

}

static int _q2_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklPseudoAxisEngine *engine = params;
	const HklPseudoAxisEngineQ2 *engine_q2 = container_of(engine, HklPseudoAxisEngineQ2, engine);
	double q;
	double alpha;

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x */
	set_geometry_axes(engine, x->data);

	_q2(engine->geometry, engine->detector, &q, &alpha);

	f->data[0] = engine_q2->q->_value - q;
	f->data[1] = engine_q2->alpha->_value - alpha;

	return  GSL_SUCCESS;
}

static const HklFunction q2_func = {
	.function = _q2_func,
	.size = 2,
};

static int get_q2_real(HklMode *self,
		       HklPseudoAxisEngine *engine,
		       HklGeometry *geometry,
		       HklDetector *detector,
		       HklSample *sample,
		       HklError **error)
{
	HklPseudoAxisEngineQ2 *engine_q2 = container_of(engine, HklPseudoAxisEngineQ2, engine);

	_q2(geometry, detector, &engine_q2->q->_value, &engine_q2->alpha->_value);

	return HKL_TRUE;
}

static HklMode *mode_q2(void)
{
	static const char* axes[] = {"gamma", "delta"};
	static const HklFunction *functions[] = {&q2_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO("q2", axes, functions),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_q2_real,
	};

	return hkl_mode_auto_new(&info, &operations);
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
	HklMode *mode;
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

/************/
/* QperQpar */
/************/

struct _HklPseudoAxisEngineQperQpar
{
	HklPseudoAxisEngine engine;
	HklParameter *qper;
	HklParameter *qpar;
};

static void _qper_qpar(HklPseudoAxisEngine *engine,
		       HklGeometry *geometry, HklDetector *detector,
		       double *qper, double *qpar)
{
	HklVector ki;
	HklVector q;
	HklVector n = {
		.data = {
			engine->mode->parameters.parameters[0]->_value,
			engine->mode->parameters.parameters[1]->_value,
			engine->mode->parameters.parameters[2]->_value,
		},
	};
	HklVector qper_v;
	HklVector qpar_v;

	/* compute q */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &q);
	hkl_vector_minus_vector(&q, &ki);

	/* compute the real orientation of n */
	hkl_vector_rotated_quaternion(&n, &geometry->holders[0].q);
	hkl_vector_normalize(&n);

	qper_v = n;
	hkl_vector_times_double(&qper_v, hkl_vector_scalar_product(&q, &n));

	qpar_v = q;
	hkl_vector_minus_vector(&qpar_v, &qper_v);

	*qper = hkl_vector_norm2(&qper_v);
	*qpar = hkl_vector_norm2(&qpar_v);
}

static int _qper_qpar_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklPseudoAxisEngine *engine = params;
	const HklPseudoAxisEngineQperQpar *engine_qper_qpar = container_of(engine, HklPseudoAxisEngineQperQpar, engine);
	double qper;
	double qpar;

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x */
	set_geometry_axes(engine, x->data);

	_qper_qpar(engine, engine->geometry, engine->detector,
		   &qper, &qpar);

	f->data[0] = engine_qper_qpar->qper->_value - qper;
	f->data[1] = engine_qper_qpar->qpar->_value - qpar;

	return  GSL_SUCCESS;
}

static const HklFunction qper_qpar_func = {
	.function = _qper_qpar_func,
	.size = 2,
};

static int get_qper_qpar_real(HklMode *self,
			      HklPseudoAxisEngine *engine,
			      HklGeometry *geometry,
			      HklDetector *detector,
			      HklSample *sample,
			      HklError **error)
{
	HklPseudoAxisEngineQperQpar *engine_qper_qpar = container_of(engine, HklPseudoAxisEngineQperQpar, engine);

	_qper_qpar(engine, geometry, detector,
		   &engine_qper_qpar->qper->_value,
		   &engine_qper_qpar->qpar->_value);

	return HKL_TRUE;
}

static HklMode *mode_qper_qpar(void)
{
	static const char* axes[] = {"gamma", "delta"};
	static const HklFunction *functions[] = {&qper_qpar_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "x", .range = {.min=-1, .max=1}, ._value = 0,},
		{HKL_PARAMETER_DEFAULTS, .name = "y", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "z", .range = {.min=-1, .max=1}, ._value = 0,},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS("qper_qpar", axes, functions, parameters),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_qper_qpar_real,
	};

	return hkl_mode_auto_new(&info, &operations);
}

static void hkl_pseudo_axis_engine_qper_qpar_free_real(HklPseudoAxisEngine *base)
{
	HklPseudoAxisEngineQperQpar *self = container_of(base, HklPseudoAxisEngineQperQpar, engine);
	hkl_pseudo_axis_engine_release(&self->engine);
	free(self);
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_qper_qpar_new(void)
{
	static const HklPseudoAxis qper = {
		.parameter = {HKL_PARAMETER_DEFAULTS, .name="qper", .range={.min=-1, .max=1}},
	};
	static const HklPseudoAxis qpar = {
		.parameter = {HKL_PARAMETER_DEFAULTS, .name="qpar", .range={.min=-1, .max=1}},
	};
	static const HklPseudoAxis *pseudo_axes[] = {&qper, &qpar};
	static const HklPseudoAxisEngineInfo info = {
		.name = "qper_qpar",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};
	static const HklPseudoAxisEngineOperations operations = {
		HKL_PSEUDO_AXIS_ENGINE_OPERATIONS_DEFAULTS,
		.free = hkl_pseudo_axis_engine_qper_qpar_free_real,
	};
	HklPseudoAxisEngineQperQpar *self;
	HklMode *mode;

	self = HKL_MALLOC(HklPseudoAxisEngineQperQpar);

	hkl_pseudo_axis_engine_init(&self->engine, &info, &operations);
	self->qper = register_pseudo_axis(&self->engine, &qper.parameter);
	self->qpar = register_pseudo_axis(&self->engine, &qpar.parameter);

	/* qper_qpar [default] */
	mode = mode_qper_qpar();
	hkl_pseudo_axis_engine_add_mode(&self->engine, mode);
	hkl_pseudo_axis_engine_select_mode(&self->engine, mode);

	return &self->engine;
}
