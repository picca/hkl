#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

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

	// update the pseudoAxes current and consign parts
	self->pseudoAxes[0].config.value = hkl.data[0];
	self->pseudoAxes[1].config.value = hkl.data[1];
	self->pseudoAxes[2].config.value = hkl.data[2];

	return HKL_SUCCESS;
}
