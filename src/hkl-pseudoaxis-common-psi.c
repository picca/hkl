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
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-pseudoaxis-common.h>
#include <hkl/hkl-pseudoaxis-auto.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

static int psi(const gsl_vector *x, void *params, gsl_vector *f)
{

	HklVector dhkl0, hkl1;
	HklVector ki, kf, Q, n;
	HklMatrix RUB;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineModePsi *modepsi;
	HklPseudoAxis *psi;
	HklHolder *holder;
	size_t i;
	size_t len;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;
	modepsi = (HklPseudoAxisEngineModePsi *)engine->mode;
	psi = engine->pseudoAxes[0];

	// update the workspace from x;
	len = HKL_LIST_LEN(engine->axes);
	for(i=0; i<len; ++i)
		hkl_axis_set_value(engine->axes[i], x_data[i]);
	hkl_geometry_update(engine->geometry);

	// kf - ki = Q
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q)){
		f_data[0] = 1;
		f_data[1] = 1;
		f_data[2] = 1;
		f_data[3] = 1;
	}else{
		// R * UB
		// for now the 0 holder is the sample holder.
		holder = &engine->geometry->holders[0];
		hkl_quaternion_to_smatrix(&holder->q, &RUB);
		hkl_matrix_times_smatrix(&RUB, &engine->sample->UB);

		// compute dhkl0
		hkl_matrix_solve(&RUB, &dhkl0, &Q);
		hkl_vector_minus_vector(&dhkl0, &modepsi->hkl0);

		// compute the intersection of the plan P(kf, ki) and PQ (normal Q)
		/* 
		 * now that dhkl0 have been computed we can use a
		 * normalized Q to compute n and psi
		 */ 
		hkl_vector_normalize(&Q);
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		// compute hkl1 in the laboratory referentiel
		// for now the 0 holder is the sample holder.
		hkl1.data[0] = engine->mode->parameters[0].value;
		hkl1.data[1] = engine->mode->parameters[1].value;
		hkl1.data[2] = engine->mode->parameters[2].value;
		hkl_vector_times_smatrix(&hkl1, &engine->sample->UB);
		hkl_vector_rotated_quaternion(&hkl1, &engine->geometry->holders[0].q);
	
		// project hkl1 on the plan of normal Q
		hkl_vector_project_on_plan(&hkl1, &Q);
		if (hkl_vector_is_null(&hkl1)){ // hkl1 colinear with Q
			f_data[0] = dhkl0.data[0];
			f_data[1] = dhkl0.data[1];
			f_data[2] = dhkl0.data[2];
			f_data[3] = 1;
		}else{
			f_data[0] = dhkl0.data[0];
			f_data[1] = dhkl0.data[1];
			f_data[2] = dhkl0.data[2];
			f_data[3] = psi->parent.value - hkl_vector_oriented_angle(&n, &hkl1, &Q);
		}
	}
	return GSL_SUCCESS;
}

static int hkl_pseudo_axis_engine_mode_init_psi_real(HklPseudoAxisEngine *engine,
							HklGeometry *geometry,
							HklDetector const *detector,
							HklSample const *sample)
{
	int status = HKL_SUCCESS;
	HklVector ki;
	HklMatrix RUB;
	HklPseudoAxisEngineModePsi *self;
	HklHolder *holder;
	
	status = hkl_pseudo_axis_engine_init_func(engine, geometry, detector, sample);
	if (status == HKL_FAIL)
		return status;

	self = (HklPseudoAxisEngineModePsi *)engine->mode;

	// update the geometry internals
	hkl_geometry_update(geometry);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = &geometry->holders[0];
	hkl_quaternion_to_smatrix(&holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, &sample->UB);

	// kf - ki = Q0
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &self->Q0);
	hkl_vector_minus_vector(&self->Q0, &ki);
	if (hkl_vector_is_null(&self->Q0))
		status = HKL_FAIL;
	else
		// compute hkl0
		hkl_matrix_solve(&RUB, &self->hkl0, &self->Q0);

	return status;
}

static int hkl_pseudo_axis_engine_mode_get_psi_real(HklPseudoAxisEngine *engine,
						       HklGeometry *geometry,
						       HklDetector const *detector,
						       HklSample const *sample)
{
	int status = HKL_SUCCESS;

	if (!engine || !engine->mode || !geometry || !detector || !sample){
		status = HKL_FAIL;
		return status;
	}

	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl1;
	HklVector n;
	HklPseudoAxisEngineModePsi *self;
	HklPseudoAxisEngineMode *base;

	self = (HklPseudoAxisEngineModePsi *)engine->mode;
	base = engine->mode;

	// get kf, ki and Q
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q))
		status = HKL_FAIL;
	else{
		hkl_vector_normalize(&Q); // needed for a problem of precision

		// compute the intersection of the plan P(kf, ki) and PQ (normal Q)
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		// compute hkl1 in the laboratory referentiel
		// the geometry was already updated in the detector compute kf
		// for now the 0 holder is the sample holder.
		hkl1.data[0] = base->parameters[0].value;
		hkl1.data[1] = base->parameters[1].value;
		hkl1.data[2] = base->parameters[2].value;
		hkl_vector_times_smatrix(&hkl1, &sample->UB);
		hkl_vector_rotated_quaternion(&hkl1, &geometry->holders[0].q);
	
		// project hkl1 on the plan of normal Q
		hkl_vector_project_on_plan(&hkl1, &Q);
	
		if (hkl_vector_is_null(&hkl1))
			status = HKL_FAIL;
		else
			// compute the angle beetween hkl1 and n
			((HklParameter *)engine->pseudoAxes[0])->value = hkl_vector_oriented_angle(&n, &hkl1, &Q);
	}

	return status;
}

static int hkl_pseudo_axis_engine_mode_set_psi_real(HklPseudoAxisEngine *engine,
						       HklGeometry *geometry,
						       HklDetector *detector,
						       HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, psi);
}

HklPseudoAxisEngineModePsi *hkl_pseudo_axis_engine_mode_psi_new(char const *name,
								     size_t axes_names_len,
								     char const *axes_names[])
{
	HklPseudoAxisEngineModePsi *self;
	char const *parameters_names[] = {"h1", "k1", "l1"};

	if (axes_names_len != 4)
		die("This generic HklPseudoAxisEngineModePsi need exactly 4 axes");

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineModePsi");

	// the base constructor;
	hkl_pseudo_axis_engine_mode_init(&self->parent,
					    name,
					    hkl_pseudo_axis_engine_mode_init_psi_real,
					    hkl_pseudo_axis_engine_mode_get_psi_real,
					    hkl_pseudo_axis_engine_mode_set_psi_real,
					    3, parameters_names,
					    axes_names_len, axes_names);

	hkl_parameter_init(&self->parent.parameters[0],
			   "h1",
			   -1, 1, 1,
			   HKL_FALSE, HKL_FALSE,
			   NULL, NULL);
	hkl_parameter_init(&self->parent.parameters[1],
			   "k1",
			   -1, 0, 1,
			   HKL_FALSE, HKL_FALSE,
			   NULL, NULL);
	hkl_parameter_init(&self->parent.parameters[2],
			   "l1",
			   -1, 0, 1,
			   HKL_FALSE, HKL_FALSE,
			   NULL, NULL);

	return self;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_psi_new(void)
{
	HklPseudoAxisEngine *self;

	self = hkl_pseudo_axis_engine_new("psi", 1, "psi");

	// psi
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "psi",
			   -M_PI, 0., M_PI,
			   HKL_FALSE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	return self;
}
