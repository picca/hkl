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
HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(
		HklPseudoAxisEngineType const *T,
		char const *name, size_t n, ...)
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
	engine->geom = NULL;
	engine->det = NULL;
	engine->sample = NULL;
	engine->related_axes_idx = NULL;
	engine->pseudoAxes = hkl_list_new_managed(&p_copy, &p_free);

	va_start(ap, n);
	for(i=0; i<n; ++i) {
		char const *p_name;
		p_name = va_arg(ap, char const *);
		HklPseudoAxis *axis = hkl_pseudoAxis_new(p_name, engine);
		hkl_list_append(engine->pseudoAxes, axis);
	}
	va_end(ap);

	engine->state = malloc(T->size);
	if(!engine->state)
		die("Can not allocate memory for an HklPseudoAxis state");

	engine->type = T;
	engine->type->alloc(engine->state, n);

	return engine;
}

void hkl_pseudoAxisEngine_set(HklPseudoAxisEngine *engine,
		HklPseudoAxisEngineFunc *function, HklGeometry *geom,
		HklDetector *det, HklSample *sample, size_t n, ...)
{
	va_list ap;
	size_t i;

	if (engine->geom)
		hkl_geometry_free(engine->geom);
	engine->geom = hkl_geometry_new_copy(geom);
	if(!engine->geom)
		die("Can not allocate memory for an HklGeometry");
	hkl_geometry_update(engine->geom);

	engine->det = det;
	engine->sample = sample;
	engine->function = function;

	if (engine->related_axes_idx)
		gsl_vector_uint_free(engine->related_axes_idx);
	engine->related_axes_idx = gsl_vector_uint_alloc(n);
	if (!engine->related_axes_idx)
		die("Cannot allocate memory for related axes idx");

	va_start(ap, n);
	for(i=0; i<n; ++i) {
		unsigned int idx;

		idx = va_arg(ap, unsigned int);
		gsl_vector_uint_set(engine->related_axes_idx, i, idx);
	}
	va_end(ap);

	(engine->type->set)(engine->state, function, engine);
}

void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *engine)
{
	if (engine->g_init)
		hkl_geometry_free(engine->g_init);
	if (engine->geom)
		hkl_geometry_free(engine->geom);
	if(engine->related_axes_idx)
		gsl_vector_uint_free(engine->related_axes_idx);
	hkl_list_free(engine->pseudoAxes);
	if(engine->state) {
		engine->type->free(engine->state);
		free(engine->state);
	}
	free(engine);
}

int RUBh_minus_Q(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklVector Hkl;
	HklVector ki, dQ;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	unsigned int i;

	engine = params;
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

	hkl_vector_init(&Hkl, H->config.value, K->config.value,
			L->config.value);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->geom, 0);
	hkl_matrix_times_vector(&engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, &holder->q);

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

int hkl_pseudoAxisEngine_to_geometry(HklPseudoAxisEngine *engine)
{
	  return (engine->type->to_geometry) (engine->state,
			  engine->function,engine);
}

int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *engine)
{
	  return (engine->type->to_pseudoAxes) (engine->state,
			  engine->function, engine);
}

int hkl_pseudoAxis_get_equiv_geometries(HklPseudoAxisEngine *engine)
{
	return (engine->type->equiv_geometries) (engine->state,
			engine->function, engine);
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
