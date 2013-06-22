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
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include "hkl/ccan/array_size/array_size.h"

#include "hkl-error-private.h"
#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-common-psi-private.h"
#include "hkl-sample-private.h"

/***********************/
/* numerical functions */
/***********************/

int _psi_func(const gsl_vector *x, void *params, gsl_vector *f)
{

	HklVector dhkl0, hkl1;
	HklVector ki, kf, Q, n;
	HklMatrix RUB;
	HklEngine *engine = params;
	HklEnginePsi *psi_engine = container_of(engine, HklEnginePsi, engine);
	HklModePsi *modepsi = container_of(engine->mode, HklModePsi, parent);
	HklHolder *sample_holder;

	CHECK_NAN(x->data, x->size);

	/* update the workspace from x; */
	set_geometry_axes(engine, x->data);

	/* kf - ki = Q */
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q)){
		f->data[0] = 1;
		f->data[1] = 1;
		f->data[2] = 1;
		f->data[3] = 1;
	}else{
		uint len;

		/* R * UB */
		/* for now the 0 holder is the sample holder. */
		sample_holder = darray_item(engine->geometry->holders, 0);
		hkl_quaternion_to_matrix(&sample_holder->q, &RUB);
		hkl_matrix_times_matrix(&RUB, &engine->sample->UB);

		/* compute dhkl0 */
		hkl_matrix_solve(&RUB, &dhkl0, &Q);
		hkl_vector_minus_vector(&dhkl0, &modepsi->hkl0);

		/* compute the intersection of the plan P(kf, ki) and PQ (normal Q) */
		/*
		 * now that dhkl0 have been computed we can use a
		 * normalized Q to compute n and psi
		 */
		hkl_vector_normalize(&Q);
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		/* compute hkl1 in the laboratory referentiel */
		/* for now the 0 holder is the sample holder. */
		hkl_parameter_list_values_get(&engine->mode->parameters, hkl1.data, &len);
		hkl_matrix_times_vector(&engine->sample->UB, &hkl1);
		hkl_vector_rotated_quaternion(&hkl1, &sample_holder->q);

		/* project hkl1 on the plan of normal Q */
		hkl_vector_project_on_plan(&hkl1, &Q);
		if (hkl_vector_is_null(&hkl1)){
			/* hkl1 colinear with Q */
			f->data[0] = dhkl0.data[0];
			f->data[1] = dhkl0.data[1];
			f->data[2] = dhkl0.data[2];
			f->data[3] = 1;
		}else{
			f->data[0] = dhkl0.data[0];
			f->data[1] = dhkl0.data[1];
			f->data[2] = dhkl0.data[2];
			f->data[3] = psi_engine->psi->_value - hkl_vector_oriented_angle(&n, &hkl1, &Q);
		}
	}
	return GSL_SUCCESS;
}

static int hkl_mode_init_psi_real(HklMode *base,
				  HklEngine *engine,
				  HklGeometry *geometry,
				  HklDetector *detector,
				  HklSample *sample,
				  HklError **error)
{
	HklVector ki;
	HklMatrix RUB;
	HklModePsi *self = container_of(base, HklModePsi, parent);
	HklHolder *sample_holder;

	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if (!hkl_mode_init_real(base, engine, geometry, detector, sample, error)){
		hkl_error_set(error, "internal error");
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	/* R * UB */
	/* for now the 0 holder is the sample holder. */
	sample_holder = darray_item(geometry->holders, 0);
	hkl_quaternion_to_matrix(&sample_holder->q, &RUB);
	hkl_matrix_times_matrix(&RUB, &sample->UB);

	/* kf - ki = Q0 */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &self->Q0);
	hkl_vector_minus_vector(&self->Q0, &ki);
	if (hkl_vector_is_null(&self->Q0)){
		hkl_error_set(error, "can not initialize the \"%s\" engine when hkl is null",
			      engine->info->name);
		return HKL_FALSE;
	}else
		/* compute hkl0 */
		hkl_matrix_solve(&RUB, &self->hkl0, &self->Q0);

	return HKL_TRUE;
}

static int hkl_mode_get_psi_real(HklMode *base,
				 HklEngine *engine,
				 HklGeometry *geometry,
				 HklDetector *detector,
				 HklSample *sample,
				 HklError **error)
{
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl1;
	HklVector n;

	if (!base || !engine || !engine->mode || !geometry || !detector || !sample){
		hkl_error_set(error, "internal error");
		return HKL_FALSE;
	}

	/* get kf, ki and Q */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q)){
		hkl_error_set(error, "can not compute psi when hkl is null (kf == ki)");
		return HKL_FALSE;
	}else{
		uint shit;

		/* needed for a problem of precision */
		hkl_vector_normalize(&Q);

		/* compute the intersection of the plan P(kf, ki) and PQ (normal Q) */
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		/* compute hkl1 in the laboratory referentiel */
		/* the geometry was already updated in the detector compute kf */
		/* for now the 0 holder is the sample holder. */
		hkl_parameter_list_values_get(&base->parameters, hkl1.data, &shit);
		hkl_matrix_times_vector(&sample->UB, &hkl1);
		hkl_vector_rotated_quaternion(&hkl1, &darray_item(geometry->holders, 0)->q);

		/* project hkl1 on the plan of normal Q */
		hkl_vector_project_on_plan(&hkl1, &Q);

		if (hkl_vector_is_null(&hkl1)){
			hkl_error_set(error, "can not compute psi when Q and the ref vector are colinear");
			return HKL_FALSE;
		}else{
			HklEnginePsi *psi_engine = container_of(engine, HklEnginePsi, engine);

			/* compute the angle beetween hkl1 and n */
			psi_engine->psi->_value = hkl_vector_oriented_angle(&n, &hkl1, &Q);
		}
	}

	return HKL_TRUE;
}

HklMode *hkl_mode_psi_new(const HklModeAutoInfo *info)
{
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_AUTO_DEFAULTS,
		.init = hkl_mode_init_psi_real,
		.get = hkl_mode_get_psi_real,
	};
	HklModePsi *self;

	if (info->mode.n_axes != 4){
		fprintf(stderr, "This generic HklModePsi need exactly 4 axes");
		exit(128);
	}

	self = HKL_MALLOC(HklModePsi);

	/* the base constructor; */
	hkl_mode_auto_init(&self->parent,
			   info,
			   &operations);

	return &self->parent;
}

/***********************/
/* HklEngine */
/***********************/

static void hkl_engine_psi_free_real(HklEngine *base)
{
	HklEnginePsi *self=container_of(base, HklEnginePsi, engine);
	hkl_engine_release(&self->engine);
	free(self);
}

HklEngine *hkl_engine_psi_new(void)
{
	HklEnginePsi *self;
	static const HklPseudoAxis psi = {
		.parameter = { HKL_PARAMETER_DEFAULTS_ANGLE, .name = "psi"}
	};
	static const HklPseudoAxis *pseudo_axes[] = {&psi};
	static const HklEngineInfo info = {
		.name = "psi",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};
	static const HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_engine_psi_free_real,
	};

	self = HKL_MALLOC(HklEnginePsi);

	hkl_engine_init(&self->engine, &info, &operations);

	self->psi = register_pseudo_axis(&self->engine, &psi.parameter);

	return &self->engine;
}
