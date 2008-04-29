#include <gsl/gsl_multimin.h>

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
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size;
	size_t i;

	// update the workspace with the right parameters
	engine->w.geom = geom;
	engine->w.det = det;
	engine->w.sample = sample;

	// Starting point
	// first update the geometry internal.
	hkl_geometry_update(engine->w.geom);

	x = gsl_vector_alloc(2 * engine->related_axes_idx->len);
	ss = gsl_vector_alloc(2 * engine->related_axes_idx->len);
	for(i=0; i<engine->related_axes_idx->len; ++i) {
		HklAxis const *axis;
		size_t *idx;

		idx = hkl_list_get_by_idx(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(geom, *idx);
		gsl_vector_set (x, 2*i, axis->config.current);
		gsl_vector_set (ss, 2*i, 0.1);
		gsl_vector_set (x, 2*i + 1, axis->config.consign);
		gsl_vector_set (ss, 2*i + 1, 0.1);
	}

	// Initialize method and iterate
	minex_func.n = 2 * engine->related_axes_idx->len;
	minex_func.f = engine->set;
	minex_func.params = engine;
	s = gsl_multimin_fminimizer_alloc(T, 2 * engine->related_axes_idx->len);
	gsl_set_error_handler_off();
	gsl_multimin_fminimizer_set (s, &minex_func, x, ss);
	do {
		++iter;
		status = gsl_multimin_fminimizer_iterate(s);
		if (status)
			break;
		size = gsl_multimin_fminimizer_size (s);
		status = gsl_multimin_test_size (size, HKL_EPSILON / 2.);
	} while (status == GSL_CONTINUE && iter < 10000);
	gsl_vector_free(x);
	gsl_vector_free(ss);
	gsl_multimin_fminimizer_free(s);
	gsl_set_error_handler (NULL);

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

double hkl_pseudoAxisEngine_hkl_set(gsl_vector const *x, void *params)
{
	HklVector Hkl, Hklc;
	HklVector ki, Q, Qc;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	HklAxis *Delta, *Omega;
	double *hkl, *hklc, res;
	unsigned int i;
	
	engine = params;
	hkl = Hkl.data;
	hklc = Hklc.data;
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
		config.current = gsl_vector_get(x, 2*i);
		config.consign = gsl_vector_get(x, 2*i + 1);
		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->w.geom);

	hkl[0] = H->config.current;
	hkl[1] = K->config.current;
	hkl[2] = L->config.current;

	hklc[0] = H->config.consign;
	hklc[1] = K->config.consign;
	hklc[2] = L->config.consign;

	Delta = hkl_geometry_get_axis(engine->w.geom, 3);
	Omega = hkl_geometry_get_axis(engine->w.geom, 0);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->w.geom, 0);
	hkl_matrix_times_vector(engine->w.sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, holder->q);
	hkl_matrix_times_vector(engine->w.sample->UB, &Hklc);
	hkl_vector_rotated_quaternion(&Hkl, holder->qc);

	// kf - ki = Q
	hkl_source_get_ki(engine->w.geom->source, &ki);
	hkl_detector_get_kf(engine->w.det, engine->w.geom, &Q, &Qc);
	hkl_vector_minus_vector(&Q, &ki);
	hkl_vector_minus_vector(&Qc, &ki);

	hkl_vector_minus_vector(&Q, &Hkl);
	hkl_vector_minus_vector(&Qc, &Hklc);

	res = hkl_vector_norm2(&Q)
		+ hkl_vector_norm2(&Qc)
		+ fabs(Delta->config.current - 2 * Omega->config.current)
		+ fabs(Delta->config.consign - 2 * Omega->config.consign);

	return res;
}
