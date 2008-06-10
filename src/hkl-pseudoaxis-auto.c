#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

typedef struct
{
	size_t n;
}
auto_state_t;

//forward declaration
static int E4CV_bissector(const gsl_vector *x, void *params, gsl_vector *f);
static int _auto_to_geometry(void *vstate, HklPseudoAxisEngine *engine);

static int auto_alloc(void *vstate, size_t n)
{
	auto_state_t *state;

	state = vstate;

	return HKL_SUCCESS;
}

static int auto_set(void *vstate, HklPseudoAxisEngine *engine)
{
	auto_state_t *state;

	state = vstate;
	state->n = engine->related_axes_idx->size;

	return HKL_SUCCESS;
}

static int auto_to_geometry(void *vstate, HklPseudoAxisEngine *engine)
{
	auto_state_t *state;

	state = vstate;
	_auto_to_geometry(vstate, engine);

	return HKL_SUCCESS;
}

static int auto_to_pseudoAxes(void *vstate, HklPseudoAxisEngine *engine)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl, ki, Q;
	HklPseudoAxis *H, *K, *L;

	// update the geometry internals
	hkl_geometry_update(engine->geom);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->geom, 0);
	hkl_quaternion_to_smatrix(holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, engine->sample->UB);

	// kf - ki = Q
	hkl_source_get_ki(engine->geom->source, &ki);
	hkl_detector_get_kf(engine->det, engine->geom, &Q);
	hkl_vector_minus_vector(&Q, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);

	// update the pseudoAxes current and consign parts
	H = hkl_list_get_by_idx(engine->pseudoAxes, 0);
	K = hkl_list_get_by_idx(engine->pseudoAxes, 1);
	L = hkl_list_get_by_idx(engine->pseudoAxes, 2);
	H->config.value = hkl.data[0];
	K->config.value = hkl.data[1];
	L->config.value = hkl.data[2];

	return HKL_SUCCESS;
}

static void auto_free(void *state)
{
}

static int _auto_to_geometry(void *vstate, HklPseudoAxisEngine *engine)
{
	auto_state_t *state;
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_vector *x;
	size_t iter = 0;
	int status;
	size_t i;
	unsigned int  idx;
	double d;

	state = vstate;
	gsl_multiroot_function f = {&E4CV_bissector, state->n, engine};

	// get the starting point from the geometry
	// must be put in the auto_set method
	x = gsl_vector_alloc(state->n);
	for(i=0; i<state->n; ++i) {
		HklAxis const *axis;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		gsl_vector_set(x, i, axis->config.value);
	}

	// Initialize method 
	T = gsl_multiroot_fsolver_hybrids;
	s = gsl_multiroot_fsolver_alloc (T, state->n);
	gsl_multiroot_fsolver_set (s, &f, x);

	// iterate to find the solution
	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		if (status || iter % 1000 == 0) {
			// Restart from another point.
			for(i=0; i<state->n; ++i) {
				d = (double)rand() / RAND_MAX * 180. / M_PI;
				gsl_vector_set(s->x, i, d);
			}
			gsl_multiroot_fsolver_set(s, &f, s->x);
			status = gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON);
	} while (status == GSL_CONTINUE && iter < 10000);

	// set the geometry from the gsl_vector
	// in a futur version the geometry must contain a gsl_vector
	// to avoid this.
	for(i=0; i<state->n; ++i) {
		HklAxisConfig config;
		HklAxis *axis;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		hkl_axis_get_config(axis, &config);
		d = gsl_vector_get(s->x, i);
		gsl_sf_angle_restrict_pos_e(&d);
		config.value = d;
		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->geom);

	// release memory
	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return HKL_SUCCESS;
}

static int common_hkl_part(const gsl_vector *x, void *params, gsl_vector *f)
{
	auto_state_t *state;
	HklVector Hkl;
	HklVector ki, dQ;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	unsigned int i;

	engine = params;
	state = engine->state;
	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);

	// update the workspace from x;
	for(i=0; i<engine->related_axes_idx->size ; ++i) {
		HklAxis *axis;
		unsigned int idx;
		HklAxisConfig config;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		hkl_axis_get_config(axis, &config);
		config.value = gsl_vector_get(x, i);

		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->geom);

	hkl_vector_set(&Hkl, H->config.value, K->config.value,
			L->config.value);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->geom, 0);
	hkl_matrix_times_vector(engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, holder->q);

	// kf - ki = Q
	hkl_source_get_ki(engine->geom->source, &ki);
	hkl_detector_get_kf(engine->det, engine->geom, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	gsl_vector_set (f, 0, dQ.data[0]);
	gsl_vector_set (f, 1, dQ.data[1]);
	gsl_vector_set (f, 2, dQ.data[2]);

	return GSL_SUCCESS;
}

static int E4CV_bissector(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, delta;

	common_hkl_part(x, params, f);

	omega = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 0));
	delta = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 3));

	gsl_vector_set (f, 3, delta - 2 * omega);

	return  GSL_SUCCESS;
}

static HklPseudoAxisEngineType auto_type =
{
	"auto",
	sizeof(auto_state_t),
	&auto_alloc,
	&auto_set,
	&auto_to_geometry,
	&auto_to_pseudoAxes,
	&auto_free
};

const HklPseudoAxisEngineType *hkl_pseudoAxisEngine_type_auto = &auto_type;
