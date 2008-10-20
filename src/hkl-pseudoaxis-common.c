#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis-common.h>
#include <hkl/hkl-pseudoaxis-auto.h>

/***************************************/
/* common methode use by getter/setter */
/***************************************/

static int RUBh_minus_Q_func(const gsl_vector *x, void *params,
		gsl_vector *f)
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
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	size_t i;

	engine = params;
	H = &engine->pseudoAxes[0];
	K = &engine->pseudoAxes[1];
	L = &engine->pseudoAxes[2];

	// update the workspace from x;
	for(i=0; i<engine->axes_len; ++i) {
		HklAxis *axis = engine->axes[i];
		axis->config.value = x[i];
		axis->config.dirty = 1;
	}
	hkl_geometry_update(engine->geometry);

	hkl_vector_init(&Hkl, H->config.value, K->config.value,
			L->config.value);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = &engine->geometry->holders[0];
	hkl_matrix_times_vector(&engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, &holder->q);

	// kf - ki = Q
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	f[0] = dQ.data[0];
	f[1] = dQ.data[1];
	f[2] = dQ.data[2];

	return GSL_SUCCESS;
}

int hkl_pseudo_axis_engine_getter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector const *detector,
		HklSample const *sample)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl, ki, Q;
	double min, max;

	// update the geometry internals
	hkl_geometry_update(geometry);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = &geometry->holders[0];
	hkl_quaternion_to_smatrix(&holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, &sample->UB);

	// kf - ki = Q
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &Q);
	hkl_vector_minus_vector(&Q, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);

	// compute the min and max
	min = -1;
	max = 1;

	// update the pseudoAxes config part
	hkl_axis_config_init(&self->pseudoAxes[0].config,
			min, max, hkl.data[0], HKL_FALSE);

	hkl_axis_config_init(&self->pseudoAxes[1].config,
			min, max, hkl.data[1], HKL_FALSE);

	hkl_axis_config_init(&self->pseudoAxes[2].config,
			min, max, hkl.data[2], HKL_FALSE);

	return HKL_SUCCESS;
}

int hkl_pseudo_axis_engine_setter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(self, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(self, RUBh_minus_Q_func);
}

