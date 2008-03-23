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
HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name, size_t n, ...)
{
	va_list ap;
	unsigned int i;
	HklPseudoAxisEngine *engine = NULL;

	engine = malloc(sizeof(*engine));
	if (!engine)
		die("Can not allocate memory for an HklPseudoAxisEngine");

	engine->name = name;
	engine->is_initialized = 0;
	engine->is_readable = 0;
	engine->is_writable = 0;
	engine->g_init = NULL;
	engine->related_axes = hkl_list_new();
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
	hkl_list_free(engine->related_axes);
	hkl_list_free(engine->pseudoAxes);
	free(engine);
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
