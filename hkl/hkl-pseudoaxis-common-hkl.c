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

/* #define DEBUG */

/*******************************************/
/* common methode use by hkl getter/setter */
/*******************************************/

typedef struct _HklDetectorFit HklDetectorFit;

struct _HklDetectorFit
{
	HklGeometry *geometry;
	HklDetector *detector;
	HklVector *kf0;
	HklAxis **axes;
	size_t len;
};

/* this method is used to fit only the detector position */
/* usable with only 1 or 2 axes */
static int fit_detector_function(const gsl_vector *x, void *params, gsl_vector *f)
{
	size_t i;
	const double *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklDetectorFit *fitp = params;
	HklVector kf;

	/* update the workspace from x; */
	for(i=0; i<fitp->len; ++i)
		hkl_axis_set_value(fitp->axes[i], x_data[i]);

	hkl_geometry_update(fitp->geometry);

	hkl_detector_compute_kf(fitp->detector, fitp->geometry, &kf);

	f_data[0] = fabs(fitp->kf0->data[0] - kf.data[0])
		+ fabs(fitp->kf0->data[1] - kf.data[1])
		+ fabs(fitp->kf0->data[2] - kf.data[2]);
	if (fitp->len > 1)
		f_data[1] = fabs(fitp->kf0->data[1] - kf.data[1]);

#if 0
	fprintf(stdout, "\nkf0 [%f, %f, %f], kf [%f, %f, %f]",
		fitp->kf0->data[0], fitp->kf0->data[1], fitp->kf0->data[2],
		kf.data[0], kf.data[1], kf.data[2]);
	fprintf(stdout, " x : [");
	for(i=0; i<fitp->len; ++i)
		fprintf(stdout, " %.7f", x_data[i]);
	fprintf(stdout, "] |  f : [");
	for(i=0; i<fitp->len; ++i)
		fprintf(stdout, " %.7f", f_data[i]);
	fprintf(stdout, "]\n");
#endif
	return GSL_SUCCESS;
}


static int fit_detector_position(HklPseudoAxisEngineMode *mode, HklGeometry *geometry,
				 HklDetector *detector, HklVector *kf)
{
	size_t i;
	HklDetectorFit params;
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_multiroot_function f;
	gsl_vector *x;
	double *x_data;
	int status;
	int res = HKL_FALSE;
	int iter;

	/* fit the detector part to find the position of the detector for a given kf */
	/* FIXME for now the sample and detector holder are respectively the first and the second one */
	/* we need to find the right axes to use for the fit */
	/* BECARFULL the sample part must not move during this fit. So exclude an axis */
	/* if it is also part of the sample holder. */ 
	/* For now compare the holder axes with the axes of the mode to generate the right gsl multiroot solver */
	params.geometry = geometry;
	params.detector = detector;
	params.kf0 = kf;
	params.axes = malloc(sizeof(*params.axes) * params.geometry->holders[1].config->len);
	params.len = 0;
	/* for each axis of the mode */
	for(i=0; i<mode->axes_names_len; ++i){
		size_t k;
		size_t tmp;

		tmp = hkl_geometry_get_axis_idx_by_name(params.geometry, mode->axes_names[i]);
		/* check that this axis is in the detector's holder */
		for(k=0; k<params.geometry->holders[1].config->len; ++k)
			if(tmp == params.geometry->holders[1].config->idx[k]){
				size_t j;
				int ko = 0;

				/* and not in the sample's holder */
				for(j=0; j<params.geometry->holders[0].config->len; ++j){
					if (tmp == params.geometry->holders[0].config->idx[j]){
						ko = 1;
						break;
					}
				}
				if(!ko)
					params.axes[params.len++] = &params.geometry->axes[tmp];
			}
	}

	/* if no detector axis found ???? abort */
	/* maybe put this at the begining of the method */
	if (params.len > 0){
		/* now solve the system */
		/* Initialize method  */
		T = gsl_multiroot_fsolver_hybrid;
		s = gsl_multiroot_fsolver_alloc (T, params.len);
		x = gsl_vector_alloc(params.len);
		x_data = gsl_vector_ptr(x, 0);

		/* initialize x with the right values */
		for(i=0; i<params.len; ++i)
			x_data[i] = hkl_axis_get_value(params.axes[i]);
	
		f.f = fit_detector_function;
		f.n = params.len;
		f.params = &params;
		gsl_multiroot_fsolver_set (s, &f, x);

		/* iterate to find the solution */
		iter = 0;
		do {
			++iter;
			status = gsl_multiroot_fsolver_iterate(s);
			if (status || iter % 100 == 0) {
				/* Restart from another point. */
				for(i=0; i<params.len; ++i)
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
			res = HKL_TRUE;
			/* put the axes in the -pi, pi range. */
			for(i=0; i<params.len; ++i)
				gsl_sf_angle_restrict_pos_e(&((HklParameter *)params.axes[i])->value);
		}
		/* release memory */
		gsl_vector_free(x);
		gsl_multiroot_fsolver_free(s);
	}
	free(params.axes);

	return res;
}

/* get the highest index of the axis in a holder */
/* BEWARE, NOT the axis index in the geometry->axes */
/* which is part of the axes_names of the mode */
/* return -1 if there is no axes of the mode in the sample part of the geometry */
static int get_last_axis_idx(HklGeometry *geometry, int holder_idx, char const **axes_names, int len)
{
	int last = -1;
	int i;
	HklHolder *holder;

	holder = &geometry->holders[holder_idx];
	for(i=0; i<len; ++i){
		size_t j;
		size_t idx;

		/* FIXME for now the sample holder is the first one */
		idx = hkl_geometry_get_axis_idx_by_name(geometry, axes_names[i]);
		for(j=0; j<holder->config->len; ++j)
			if(idx == holder->config->idx[j]){
				last = last > (int)j ? last : (int)j;
				break;
			}
	}
	return last;
}

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
	for(i=0; i<engine->axes_len; ++i)
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
	for(i=0;i<engine->pseudoAxes_len;++i){
		HklParameter *parameter = (HklParameter *)(engine->pseudoAxes[i]);
		parameter->value = hkl.data[i];
		parameter->range.min = min;
		parameter->range.max = max;
	}

	return HKL_TRUE;
}

int hkl_pseudo_axis_engine_mode_set_hkl_real(HklPseudoAxisEngineMode *self,
					     HklPseudoAxisEngine *engine,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample,
					     HklError **error)
{
	int last_axis;

	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!hkl_pseudo_axis_engine_mode_set_real(self, engine,
						 geometry, detector, sample,
						 error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	/* check that the mode allow to move a sample axis */
	/* FIXME for now the sample holder is the first one */
	last_axis = get_last_axis_idx(geometry, 0, self->axes_names, self->axes_names_len);
	if(last_axis >= 0){
		int i;
		int len;
		
		/* For each solution already found we will generate another one */
		/* using the Ewalds construction by rotating Q around the last sample */
		/* axis of the mode until it intersect again the Ewald sphere. */
		/* FIXME do not work if ki is colinear with the axis. */

		/* for this we needs : */
		/* - the coordinates of the end of the Q vector (q) */
		/* - the last sample axis orientation of the mode (axis_v) */
		/* - the coordinates of the center of the ewalds sphere (c) */
		/* - the coordinates of the center of rotation of the sample (o = 0, 0, 0) */

		/* then we can : */
		/* - project the origin in plane of normal axis_v containing q (o') */
		/* - project the center of the ewalds sphere into the same plan (c') */
		/* - rotate q around this (o', c') line of 180° to find the (q2) solution */
		/* - compute the (kf2) corresponding to this q2 solution */
		/* at the end we just need to solve numerically the position of the detector */

		/* we will add solution to the geometries so save its length before */
		len = engine->engines->geometries->len;
		for(i=0; i<len; ++i){
			int j;
			HklGeometry *geom;
			HklVector ki;
			HklVector kf;
			HklVector kf2;
			HklVector q;
			HklVector axis_v;
			HklQuaternion qr;
			HklAxis *axis;
			HklVector cp = {0};
			HklVector op = {0};
			double angle;

			geom = hkl_geometry_new_copy(engine->engines->geometries->items[i].geometry);

			/* get the Q vector kf - ki */
			hkl_detector_compute_kf(detector, geom, &q);
			hkl_source_compute_ki(&geom->source, &ki);
			hkl_vector_minus_vector(&q, &ki);

			/* compute the current orientation of the last axis */
			axis = &geom->axes[geom->holders[0].config->idx[last_axis]];
			axis_v = axis->axis_v;
			hkl_quaternion_init(&qr, 1, 0, 0, 0);
			for(j=0; j<last_axis; ++j)
				hkl_quaternion_times_quaternion(&qr, &geom->axes[geom->holders[0].config->idx[j]].q);
			hkl_vector_rotated_quaternion(&axis_v, &qr);

			/* - project the center of the ewalds sphere into the same plan (c') */
			hkl_vector_minus_vector(&cp, &ki);
			hkl_vector_project_on_plan_with_point(&cp, &axis_v, &q);
			hkl_vector_project_on_plan_with_point(&op, &axis_v, &q);

				
			/* - rotate q around this (o', c') line of 180° to find the (q2) solution */
			kf2 = q;
			hkl_vector_rotated_around_line(&kf2, M_PI, &cp, &op);
			angle = hkl_vector_oriented_angle_points(&q, &op, &kf2, &axis_v);
			hkl_axis_set_value(axis, ((HklParameter *)axis)->value + angle);
			hkl_geometry_update(geom);
#ifdef DEBUG
			fprintf(stdout, "\n- try to add a solution by rotating Q <%f, %f, %f> around the \"%s\" axis <%f, %f, %f> of %f radian",
				q.data[0], q.data[1], q.data[2],
				((HklParameter *)axis)->name,
				axis_v.data[0], axis_v.data[1], axis_v.data[2],
				angle);
			fprintf(stdout, "\n   op: <%f, %f, %f>", op.data[0], op.data[1], op.data[2]);
			fprintf(stdout, "\n   q2: <%f, %f, %f>", kf2.data[0], kf2.data[1], kf2.data[2]);
#endif
			hkl_vector_add_vector(&kf2, &ki);
			/* at the end we just need to solve numerically the position of the detector */
			if(fit_detector_position(self, geom, detector, &kf2))
				hkl_geometry_list_add(engine->engines->geometries, geom);

			hkl_geometry_free(geom);
		}
	}
	return HKL_TRUE;
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
	for(i=0; i<engine->axes_len; ++i)
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
	HklVector ki, kf, Q;
	HklPseudoAxisEngine *engine;
	size_t i;

	RUBh_minus_Q(x_data, params, f_data);
	engine = params;

	/* update the workspace from x; */
	for(i=0; i<engine->axes_len; ++i)
		hkl_axis_set_value(engine->axes[i], x_data[i]);
	hkl_geometry_update(engine->geometry);

	/* kf - ki = Q */
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);

	f_data[3] =  engine->mode->parameters[3].value;

	/* if |Q| > epsilon ok */
	if(hkl_vector_normalize(&Q)){
		HklVector hkl;
		HklVector n;

		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		hkl.data[0] = engine->mode->parameters[0].value;
		hkl.data[1] = engine->mode->parameters[1].value;
		hkl.data[2] = engine->mode->parameters[2].value;
		hkl_vector_times_matrix(&hkl, &engine->sample->UB);
		hkl_vector_rotated_quaternion(&hkl, &engine->geometry->holders[0].q);

		/* project hkl on the plan of normal Q */
		hkl_vector_project_on_plan(&hkl, &Q);
#if DEBUG
		hkl_geometry_fprintf(stdout, engine->geometry);
		fprintf(stdout, "%s n : <%f, %f, %f> hkl : <%f, %f, %f> Q : <%f, %f, %f>\n",
			__func__,
			n.data[0], n.data[1], n.data[2],
			hkl.data[0], hkl.data[1], hkl.data[2],
			Q.data[0], Q.data[1], Q.data[2]);
#endif
		if(hkl_vector_norm2(&hkl) > HKL_EPSILON)
			f_data[3] -=  hkl_vector_oriented_angle(&n, &hkl, &Q);
	}

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

	if (!self || !engine || !engine->mode || !geometry || !detector || !sample
	    || !hkl_pseudo_axis_engine_init_func(self, engine, geometry, detector, sample)){
		hkl_error_set(error, "internal error");
		return HKL_FALSE;
	}

	/* kf - ki = Q */
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);

	if (hkl_vector_is_null(&Q)){
		hkl_error_set(error, "can not initialize the \"%s\" mode with a null hkl (kf == ki)"
			      "\nplease select a non-null hkl", engine->mode->name);
		return HKL_FALSE;
	}else{
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

		if (hkl_vector_is_null(&hkl)){
			hkl_error_set(error, "can not initialize the \"%s\" mode"
				      "\nwhen Q and the <h2, k2, l2> ref vector are colinear."
				      "\nplease change one or both of them", engine->mode->name);
			return HKL_FALSE;
		}else
			/* compute the angle beetween hkl and n and
			 * store in in the fourth parameter */
			hkl_parameter_set_value(&self->parameters[3],
						hkl_vector_oriented_angle(&n, &hkl, &Q));
	}

	return HKL_TRUE;
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
