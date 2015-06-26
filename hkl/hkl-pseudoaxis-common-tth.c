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
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_errno.h>              // for ::GSL_SUCCESS
#include <gsl/gsl_sf_trig.h>            // for gsl_sf_angle_restrict_symm
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include <math.h>                       // for sin, atan2, signbit
#include <stdlib.h>                     // for free
#include "hkl-detector-private.h"       // for hkl_detector_compute_kf
#include "hkl-geometry-private.h"       // for _HklGeometry, HklHolder
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-pseudoaxis-auto-private.h"  // for HklFunction, etc
#include "hkl-pseudoaxis-common-tth-private.h"  // for HklEngineQ2, etc
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, etc
#include "hkl-source-private.h"         // for hkl_source_compute_ki, etc
#include "hkl-vector-private.h"         // for HklVector, hkl_vector_angle, etc
#include "hkl.h"                        // for HklEngine, HklParameter, etc
#include "hkl/ccan/array_size/array_size.h"  // for ARRAY_SIZE
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_item


/********/
/* tth2 */
/********/

struct _HklEngineTth2
{
	HklEngine engine;
	HklParameter *tth;
	HklParameter *alpha;
};

static void _tth2(HklGeometry *geometry, HklDetector *detector,
		  double *tth, double *alpha)
{
	HklVector kf, ki;
	static HklVector x = {
		.data = {1, 0, 0},
	};

	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	*tth = hkl_vector_angle(&ki, &kf);

	/* project kf on the x plan to compute alpha */
	hkl_vector_project_on_plan(&kf, &x);

	*alpha = atan2(kf.data[2], kf.data[1]);

}

static int _tth2_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklEngine *engine = params;
	const HklEngineTth2 *engine_tth2 = container_of(engine, HklEngineTth2, engine);
	double tth;
	double alpha;

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x */
	set_geometry_axes(engine, x->data);

	_tth2(engine->geometry, engine->detector, &tth, &alpha);

	f->data[0] = engine_tth2->tth->_value - tth;
	f->data[1] = engine_tth2->alpha->_value - alpha;

	return  GSL_SUCCESS;
}

static const HklFunction tth2_func = {
	.function = _tth2_func,
	.size = 2,
};

static int get_tth2_real(HklMode *self,
			 HklEngine *engine,
			 HklGeometry *geometry,
			 HklDetector *detector,
			 HklSample *sample,
			 GError **error)
{
	HklEngineTth2 *engine_tth2 = container_of(engine, HklEngineTth2, engine);

	_tth2(geometry, detector,
	      &engine_tth2->tth->_value,
	      &engine_tth2->alpha->_value);

	return TRUE;
}

static HklMode *mode_tth2(void)
{
	static const char* axes[] = {"gamma", "delta"};
	static const HklFunction *functions[] = {&tth2_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("tth2", axes, axes, functions),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_AUTO_DEFAULTS,
		.get = get_tth2_real,
	};

	return hkl_mode_auto_new(&info, &operations, TRUE);
}

static void hkl_engine_tth2_free_real(HklEngine *base)
{
	HklEngineTth2 *self = container_of(base, HklEngineTth2, engine);
	hkl_engine_release(&self->engine);
	free(self);
}

HklEngine *hkl_engine_tth2_new(HklEngineList *engines)
{
	HklEngineTth2 *self;
	HklMode *mode;

	static const HklParameter tth = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name="tth",
		.description = "the $2 \\theta$ angle",
	};
	static const HklParameter alpha = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "alpha",
		.description = "angle of the projection of $\\vec{q}$ on the $yOz$ plan and $\\vec{y}$",
	};
	static const HklParameter *pseudo_axes[] = {&tth, &alpha};
	static const HklEngineInfo info = {
		HKL_ENGINE_INFO("tth2",
				pseudo_axes,
				HKL_ENGINE_DEPENDENCIES_AXES),
	};
	static const HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_engine_tth2_free_real,
	};

	self = HKL_MALLOC(HklEngineTth2);

	hkl_engine_init(&self->engine, &info, &operations, engines);
	self->tth = register_pseudo_axis(&self->engine, engines, &tth);
	self->alpha = register_pseudo_axis(&self->engine, engines, &alpha);

	/* tth2 [default] */
	mode = mode_tth2();
	hkl_engine_add_mode(&self->engine, mode);
	hkl_engine_mode_set(&self->engine, mode);

	return &self->engine;
}
