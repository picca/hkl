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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 */
#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-pseudoaxis-common.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>
#include <hkl/hkl-pseudoaxis-auto.h>
/***************************************/
/* common methode use by getter/setter */
/***************************************/

int RUBh_minus_Q_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	return  GSL_SUCCESS;
}

int RUBh_minus_Q(double const x[], void *params, double f[])
{
	HklVector Hkl;
	HklVector ki, dQ;
	HklPseudoAxisEngine *engine;
	HklHolder *holder;
	size_t i;

	engine = params;

	/* update the workspace from x; */
	for(i=0; i<HKL_LIST_LEN(engine->axes); ++i)
		hkl_axis_set_value(engine->axes[i], x[i]);
	hkl_geometry_update(engine->geometry);

	hkl_vector_init(&Hkl,
			((HklParameter *)engine->pseudoAxes[0])->value,
			((HklParameter *)engine->pseudoAxes[1])->value,
			((HklParameter *)engine->pseudoAxes[2])->value);

	/* R * UB * h = Q */
	/* for now the 0 holder is the sample holder. */
	holder = &engine->geometry->holders[0];
	hkl_matrix_times_vector(&engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, &holder->q);

	/* kf - ki = Q */
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	f[0] = dQ.data[0];
	f[1] = dQ.data[1];
	f[2] = dQ.data[2];

	return GSL_SUCCESS;
}

int hkl_pseudo_axis_engine_mode_get_hkl_real(HklPseudoAxisEngineMode *self,
					     HklPseudoAxisEngine *engine,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample,
					     HklError **error)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl, ki, Q;
	double min, max;
	size_t i;

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	/* R * UB */
	/* for now the 0 holder is the sample holder. */
	holder = &geometry->holders[0];
	hkl_quaternion_to_matrix(&holder->q, &RUB);
	hkl_matrix_times_matrix(&RUB, &sample->UB);

	/* kf - ki = Q */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &Q);
	hkl_vector_minus_vector(&Q, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);

	/* compute the min and max */
	min = -1;
	max = 1;

	/* update the pseudoAxes config part */
	for(i=0;i<HKL_LIST_LEN(engine->pseudoAxes);++i){
		HklParameter *parameter = (HklParameter *)(engine->pseudoAxes[i]);
		parameter->value = hkl.data[i];
		parameter->range.min = min;
		parameter->range.max = max;
	}

	return HKL_SUCCESS;
}

/***************************************/
/* the double diffraction get set part */
/***************************************/

int double_diffraction_func(gsl_vector const *x, void *params, gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	double_diffraction(x_data, params, f_data);

	return  GSL_SUCCESS;
}

int double_diffraction(double const x[], void *params, double f[])
{
	HklPseudoAxisEngine *engine = params;
	HklVector hkl, kf2;
	HklVector ki;
	HklVector dQ;
	size_t i;
	HklHolder *holder;

	/* update the workspace from x; */
	for(i=0; i<HKL_LIST_LEN(engine->axes); ++i)
		hkl_axis_set_value(engine->axes[i], x[i]);
	hkl_geometry_update(engine->geometry);

	hkl_vector_init(&hkl,
			((HklParameter *)engine->pseudoAxes[0])->value,
			((HklParameter *)engine->pseudoAxes[1])->value,
			((HklParameter *)engine->pseudoAxes[2])->value);

	hkl_vector_init(&kf2,
			engine->mode->parameters[0].value,
			engine->mode->parameters[1].value,
			engine->mode->parameters[2].value);

	/* R * UB * hkl = Q */
	/* for now the 0 holder is the sample holder. */
	holder = &engine->geometry->holders[0];
	hkl_matrix_times_vector(&engine->sample->UB, &hkl);
	hkl_vector_rotated_quaternion(&hkl, &holder->q);

	/* kf - ki = Q */
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);
	hkl_vector_minus_vector(&dQ, &hkl);

	/* R * UB * hlk2 = Q2 */
	hkl_matrix_times_vector(&engine->sample->UB, &kf2);
	hkl_vector_rotated_quaternion(&kf2, &holder->q);
	hkl_vector_add_vector(&kf2, &ki);

	f[0] = dQ.data[0];
	f[1] = dQ.data[1];
	f[2] = dQ.data[2];
	f[3] = hkl_vector_norm2(&kf2) - hkl_vector_norm2(&ki);

	return GSL_SUCCESS;
}

/******************************************/
/* the psi_constant_vertical get set part */
/******************************************/

int psi_constant_vertical_func(gsl_vector const *x, void *params, gsl_vector *f)
{
       
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	HklVector hkl;
	HklVector ki, kf, Q, n;
	HklPseudoAxisEngine *engine;
	size_t i;

	RUBh_minus_Q(x_data, params, f_data);
	engine = params;

	/* update the workspace from x; */
	for(i=0; i<HKL_LIST_LEN(engine->axes); ++i)
		hkl_axis_set_value(engine->axes[i], x_data[i]);
	hkl_geometry_update(engine->geometry);

	hkl_vector_init(&hkl, 1, 0, 0);

	/* kf - ki = Q */
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);

	hkl_vector_normalize(&Q);
	n = kf;
	hkl_vector_vectorial_product(&n, &ki);
	hkl_vector_vectorial_product(&n, &Q);

	
	hkl_vector_times_matrix(&hkl, &engine->sample->UB);
	hkl_vector_rotated_quaternion(&hkl, &engine->geometry->holders[0].q);

	/* project hkl on the plan of normal Q */
	hkl_vector_project_on_plan(&hkl, &Q);

	f_data[3] =  engine->mode->parameters[3].value - hkl_vector_oriented_angle(&n, &hkl, &Q);

	return  GSL_SUCCESS;
}

int hkl_pseudo_axis_engine_mode_init_psi_constant_vertical_real(HklPseudoAxisEngineMode *self,
								HklPseudoAxisEngine *engine,
								HklGeometry *geometry,
								HklDetector *detector,
								HklSample *sample,
								HklError **error)
{
	HklVector hkl;
	HklVector ki, kf, Q, n;
	int status = HKL_SUCCESS;

	if (!self || !engine || !engine->mode || !geometry || !detector || !sample){
		status = HKL_FAIL;
		return status;
	}

	status = hkl_pseudo_axis_engine_init_func(self, engine, geometry, detector, sample);
	if(status == HKL_FAIL)
		return status;

	/* Compute the constant psi value (to be kept) */
	hkl_vector_init(&hkl, 1, 0, 0);

	/* kf - ki = Q */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);

	if (hkl_vector_is_null(&Q))
		status = HKL_FAIL;
	else{
		/* needed for a problem of precision */
		hkl_vector_normalize(&Q);

		/* compute the intersection of the plan P(kf, ki) and PQ (normal Q) */
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		/* compute hkl in the laboratory referentiel */
		/* the geometry was already updated in the detector compute kf */
		/* for now the 0 holder is the sample holder */
		hkl.data[0] = self->parameters[0].value;
		hkl.data[1] = self->parameters[1].value;
		hkl.data[2] = self->parameters[2].value;
		hkl_vector_times_matrix(&hkl, &sample->UB);
		hkl_vector_rotated_quaternion(&hkl, &geometry->holders[0].q);
	
		/* project hkl on the plan of normal Q */
		hkl_vector_project_on_plan(&hkl, &Q);

		if (hkl_vector_is_null(&hkl))
			status = HKL_FAIL;
		else
			/* compute the angle beetween hkl and n and
			 * store in in the fourth parameter */
			hkl_parameter_set_value(&self->parameters[3],
						hkl_vector_oriented_angle(&n, &hkl, &Q));
	}

	return status;
}


HklPseudoAxisEngine *hkl_pseudo_axis_engine_hkl_new(void)
{
	HklPseudoAxisEngine *self;

	self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");

	/* h */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "h",
			   -1, 0., 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);
	/* k */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[1],
			   "k",
			   -1, 0., 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);
	/* l */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[2],
			   "l",
			   -1, 0., 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	return self;
}
