#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

/* private */
static HklPseudoAxis *hkl_pseudoAxis_new(char const *name,
		HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *axis = NULL;
	axis = malloc(sizeof(*axis));
	if(!axis)
		die("Can not allocate memory for a HklPseudoAxis.");
	axis->name = name;
	axis->engine = engine;
	return axis;
}

static void hkl_pseudoAxis_free(HklPseudoAxis *axis)
{
	free(axis);
}

static void *d_copy(void const *p)
{
	size_t const *idx = p;
	size_t *copy = malloc(sizeof(size_t));
	*copy = *idx;

	return copy;
}

static void *p_copy(void const *p)
{
	HklPseudoAxis const *axis = p;
	HklPseudoAxis *copy = hkl_pseudoAxis_new(axis->name, axis->engine);
	return copy;
}

static void p_free(void *p)
{
	HklPseudoAxis *axis = p;
	hkl_pseudoAxis_free(axis);
}

/* public */
HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name, size_t n, ...)
{
	va_list ap;
	size_t i;
	HklPseudoAxisEngine *engine = NULL;

	engine = malloc(sizeof(*engine));
	if (!engine)
		die("Can not allocate memory for an HklPseudoAxisEngine");

	engine->name = name;
	engine->is_initialized = 0;
	engine->is_readable = 0;
	engine->is_writable = 0;
	engine->g_init = NULL;
	engine->related_axes_idx = hkl_list_new_managed(&d_copy, &free);
	engine->pseudoAxes = hkl_list_new_managed(&p_copy, &p_free);
	engine->init = NULL;
	engine->update = NULL;
	engine->set = NULL;

	va_start(ap, n);
	for(i=0; i<n; ++i) {
		char const *p_name;
		p_name = va_arg(ap, char const *);
		HklPseudoAxis *axis = hkl_pseudoAxis_new(p_name, engine);
		hkl_list_append(engine->pseudoAxes, axis);
	}
	va_end(ap);

	return engine;
}

void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *engine)
{
	if (engine->g_init)
		hkl_geometry_free(engine->g_init);
	hkl_list_free(engine->related_axes_idx);
	hkl_list_free(engine->pseudoAxes);
	free(engine);
}

void hkl_pseudoAxisEngine_set_related_axes(HklPseudoAxisEngine *engine,
		size_t n, ...)
{
	va_list ap;
	size_t i;

	va_start(ap, n);
	for(i=0; i<n; ++i) {
		size_t *idx;

		idx = malloc(sizeof(size_t));
		*idx = va_arg(ap, size_t);
		hkl_list_append(engine->related_axes_idx, idx);
	}
	va_end(ap);
}

int hkl_pseudoAxisEngine_from_pseudoAxes(HklPseudoAxisEngine *engine,
		HklGeometry *geom, HklDetector *det, HklSample *sample)
{
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_vector *x;
	size_t iter = 0;
	int status;
	size_t i;
	size_t n;

	// update the workspace with the right parameters
	engine->w.geom = geom;
	engine->w.det = det;
	engine->w.sample = sample;

	// first update the geometry internal.
	hkl_geometry_update(engine->w.geom);

	n = engine->related_axes_idx->len;
	gsl_multiroot_function f = {engine->set, n, engine};

	// get the starting point from the geometry
	x = gsl_vector_alloc(n);
	for(i=0; i<n; ++i) {
		HklAxis const *axis;
		size_t *idx;

		idx = hkl_list_get_by_idx(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(geom, *idx);
		gsl_vector_set(x, i, axis->config.current);
	}

	// Initialize method and iterate
	T = gsl_multiroot_fsolver_hybrids;
	s = gsl_multiroot_fsolver_alloc (T, n);
	gsl_multiroot_fsolver_set (s, &f, x);

	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		//for(i=0; i<n; ++i)
		//	gsl_sf_angle_restrict_pos_e(gsl_vector_ptr(s->x, i));
		if (status || iter % 1000 == 0) {

			// Restart from another point.
			for(i=0; i<n; ++i) {
				double x;
				
				x = (double)rand() / RAND_MAX * 180. / M_PI;
				gsl_vector_set(s->x, i, x);
			}
			gsl_multiroot_fsolver_set(s, &f, s->x);
			status = gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON);
	} while (status == GSL_CONTINUE && iter < 10000);

	// set the geometry from the gsl_vector
	// in a futur version the geometry must contain a gsl_vector
	// to avoid this.
	for(i=0; i<n; ++i) {
		HklAxis *axis;
		HklAxisConfig config;
		size_t *idx;

		idx = hkl_list_get_by_idx(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->w.geom, *idx);
		hkl_axis_get_config(axis, &config);
		config.current = gsl_vector_get(s->x, i);
		gsl_sf_angle_restrict_pos_e(&config.current);
		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->w.geom);

	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return HKL_SUCCESS;
}

int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *engine,
		HklGeometry *geom, HklDetector *det, HklSample *sample)
{
	// set the workspace
	engine->w.geom = geom;
	engine->w.det = det;
	engine->w.sample = sample;

	// update the geometry internals
	hkl_geometry_update(geom);

	if (engine->update)
		return engine->update(engine);
	else
		return -1;
}

HklPseudoAxis *hkl_pseudoAxisEngine_get_pseudoAxis(
		HklPseudoAxisEngine const *engine, size_t idx)
{
	return hkl_list_get_by_idx(engine->pseudoAxes, idx);
}

void hkl_pseudoAxis_get_config(HklPseudoAxis const *pseudoAxis,
		HklAxisConfig *config)
{
	*config = pseudoAxis->config;
}

void hkl_pseudoAxis_set_config(HklPseudoAxis *pseudoAxis,
		HklAxisConfig const *config)
{
	pseudoAxis->config = *config;
}

int hkl_pseudoAxisEngine_hkl_update(HklPseudoAxisEngine *engine)
{
	HklHolder *holder;
	HklMatrix RUB, RUBc;
	HklVector hkl, hklc, ki, Q, Qc;
	HklPseudoAxis *H, *K, *L;

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->w.geom, 0);
	hkl_quaternion_to_smatrix(holder->q, &RUB);
	hkl_quaternion_to_smatrix(holder->qc, &RUBc);
	hkl_matrix_times_smatrix(&RUB, engine->w.sample->UB);
	hkl_matrix_times_smatrix(&RUBc, engine->w.sample->UB);

	// kf - ki = Q
	hkl_source_get_ki(engine->w.geom->source, &ki);
	hkl_detector_get_kf(engine->w.det, engine->w.geom, &Q, &Qc);
	hkl_vector_minus_vector(&Q, &ki);
	hkl_vector_minus_vector(&Qc, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);
	hkl_matrix_solve(&RUBc, &hklc, &Qc);

	// update the pseudoAxes current and consign parts
	H = hkl_list_get_by_idx(engine->pseudoAxes, 0);
	K = hkl_list_get_by_idx(engine->pseudoAxes, 1);
	L = hkl_list_get_by_idx(engine->pseudoAxes, 2);
	H->config.current = hkl.data[0];
	K->config.current = hkl.data[1];
	L->config.current = hkl.data[2];
	H->config.consign = hklc.data[0];
	K->config.consign = hklc.data[1];
	L->config.consign = hklc.data[2];

	return 0;
}

int hkl_pseudoAxisEngine_hkl_set(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	HklVector Hkl;
	HklVector ki, dQ, Qc;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	double omega, delta;
	unsigned int i;

	engine = params;
	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);

	// update the workspace from x;
	for(i=0; i<engine->related_axes_idx->len ; ++i) {
		HklAxis *axis;
		size_t *idx;
		HklAxisConfig config;

		idx = hkl_list_get_by_idx(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->w.geom, *idx);
		hkl_axis_get_config(axis, &config);
		config.current = gsl_vector_get(x, i);

		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->w.geom);

	hkl_vector_set(&Hkl, H->config.current, K->config.current,
			L->config.current);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->w.geom, 0);
	hkl_matrix_times_vector(engine->w.sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, holder->q);

	// kf - ki = Q
	hkl_source_get_ki(engine->w.geom->source, &ki);
	hkl_detector_get_kf(engine->w.det, engine->w.geom, &dQ, &Qc);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	gsl_vector_set (f, 0, dQ.data[0]);
	gsl_vector_set (f, 1, dQ.data[1]);
	gsl_vector_set (f, 2, dQ.data[2]);

	omega = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 0));
	delta = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 3));

	gsl_vector_set (f, 3, delta - 2 * omega);

	return  GSL_SUCCESS;
}
