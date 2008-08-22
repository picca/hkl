#include <math.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-geometry.h>

/* public part */

HklGeometry *hkl_geometry_new(void)
{
	HklGeometry *g = NULL;

	g = malloc(sizeof(*g));
	if (!g)
		die("Cannot allocate a HklGeometry struct !!!");

	hkl_source_init(&g->source, 1.54, 1, 0, 0);
	g->axes = hkl_list_new();
	g->holders = NULL;
	g->holders_len = 0;

	return g;
}

HklGeometry *hkl_geometry_new_copy(HklGeometry const *src)
{
	HklGeometry *copy = NULL;
	unsigned int i;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Cannot allocate a HklGeometry struct !!!");

	copy->source = src->source;

	// copy the axes
	copy->axes = hkl_list_new();
	for(i=0; i<src->axes->len; ++i) {
		HklAxis *axis = hkl_axis_new_copy(src->axes->list[i]);
		hkl_list_append(copy->axes, axis);
	}

	// copy the holders
	copy->holders = malloc(src->holders_len * sizeof(HklHolder));
	copy->holders_len = src->holders_len;
	for(i=0; i<src->holders_len; ++i)
		hkl_holder_init_copy(&copy->holders[i], copy,
				&src->holders[i]);

	return copy;
}

void hkl_geometry_free(HklGeometry *g)
{
	unsigned int i;

	for(i=0; i<g->axes->len; ++i)
		hkl_axis_free(g->axes->list[i]);
	hkl_list_free(g->axes);

	if (g->holders)
		free(g->holders), g->holders = NULL, g->holders_len = 0;

	free(g);
}

HklHolder *hkl_geometry_add_holder(HklGeometry *g)
{
	HklHolder *holder;

	g->holders = realloc( g->holders, (g->holders_len+1)*sizeof(HklHolder));
	holder = &g->holders[g->holders_len++];
	hkl_holder_init(holder, g);

	return holder;
}

HklAxis *hkl_geometry_get_axis(HklGeometry *g, size_t idx)
{
	HklAxis *axis = NULL;
	if (idx < g->axes->len)
		axis = g->axes->list[idx];

	return axis;
}

HklAxis const *hkl_geometry_get_axis_const(HklGeometry const *g, size_t idx)
{
	HklAxis const *axis = NULL;
	if (idx < g->axes->len)
		axis = g->axes->list[idx];

	return axis;
}

void hkl_geometry_update(HklGeometry *g)
{
	size_t i;

	for(i=0; i<g->holders_len; i++)
		hkl_holder_update(&g->holders[i]);

	for(i=0; i<g->axes->len; i++)
		hkl_axis_clear_dirty((HklAxis *)g->axes->list[i]);
}

void hkl_geometry_fprintf(FILE *file, HklGeometry const *g)
{
	size_t i;
	HklAxis const *axis;
	double value;

	for(i=0; i<g->axes->len; ++i) {
		axis = hkl_list_get_by_idx(g->axes, i);
		value = axis->config.value;
		value *= HKL_RADTODEG;
		fprintf(file, " %s : %f", axis->name, value);
	}
}
