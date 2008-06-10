#include <math.h>
#include <hkl/hkl-geometry.h>

/* public part */

HklGeometry *hkl_geometry_new(void)
{
	HklGeometry *g = NULL;

	g = malloc(sizeof(*g));
	if (!g)
		die("Cannot allocate a HklGeometry struct !!!");

	g->source  = hkl_source_new(1.54, 1, 0, 0);
	g->axes = hkl_list_new();
	g->holders = hkl_list_new();

	return g;
}

HklGeometry *hkl_geometry_new_copy(HklGeometry const *src)
{
	HklGeometry *copy = NULL;
	unsigned int i;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Cannot allocate a HklGeometry struct !!!");

	copy->source = hkl_source_new_copy(src->source);

	// copy the axes
	copy->axes = hkl_list_new();
	for(i=0; i<src->axes->len; ++i) {
		HklAxis *axis = hkl_axis_new_copy(src->axes->list[i]);
		hkl_list_append(copy->axes, axis);
	}

	// copy the holders
	copy->holders = hkl_list_new();
	for(i=0; i<src->holders->len; ++i) {
		HklHolder *holder;
		
		holder = hkl_holder_new_copy(src->holders->list[i], copy->axes);
		hkl_list_append(copy->holders, holder);
	}

	return copy;
}

void hkl_geometry_free(HklGeometry *g)
{
	unsigned int i;

	hkl_source_free(g->source);

	for(i=0; i<g->axes->len; ++i)
		hkl_axis_free(g->axes->list[i]);
	hkl_list_free(g->axes);

	for(i=0; i<g->holders->len; ++i)
		hkl_holder_free(g->holders->list[i]);
	hkl_list_free(g->holders);

	free(g);
}

HklHolder *hkl_geometry_add_holder(HklGeometry *g)
{
	HklHolder *holder = hkl_holder_new(g->axes);
	hkl_list_append(g->holders, holder);

	return holder;
}

HklHolder *hkl_geometry_get_holder(HklGeometry const *g, size_t idx)
{
	HklHolder *holder = NULL;
	if (idx < g->holders->len)
		holder = g->holders->list[idx];
	
	return holder;
}

HklAxis *hkl_geometry_get_axis(HklGeometry *g, size_t idx)
{
	HklAxis *axis = NULL;
	if (idx < g->axes->len)
		axis = g->axes->list[idx];

	return axis;
}

void hkl_geometry_update(HklGeometry *g)
{
	size_t i;

	for(i=0; i<g->holders->len; i++)
		hkl_holder_update((HklHolder *)g->holders->list[i]);
	
	for(i=0; i<g->axes->len; i++)
		hkl_axis_clear_dirty((HklAxis *)g->axes->list[i]);
}

void hkl_geometry_fprintf(FILE *file, HklGeometry const *g)
{
	size_t i;
	HklAxis *axis;
	double value;

	for(i=0; i<g->axes->len; ++i) {
		axis = hkl_list_get_by_idx(g->axes, i);
		value = axis->config.value * HKL_RADTODEG;
		fprintf(stdout, " %s : %f", axis->name, value);
	}
}
