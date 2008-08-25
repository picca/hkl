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
	g->axes = NULL;
	g->axes_len = 0;
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
	copy->axes = malloc(src->axes_len * sizeof(HklAxis*));
	copy->axes_len = src->axes_len;
	for(i=0; i<src->axes_len; ++i) {
		HklAxis *axis = malloc(sizeof(HklAxis));
		*axis = *src->axes[i];
		copy->axes[i] = axis;
	}

	// copy the holders
	copy->holders = malloc(src->holders_len * sizeof(HklHolder));
	copy->holders_len = src->holders_len;
	for(i=0; i<src->holders_len; ++i)
		hkl_holder_init_copy(&copy->holders[i], copy,
				&src->holders[i]);

	return copy;
}

void hkl_geometry_free(HklGeometry *self)
{
	size_t i;

	if(self->axes_len) {
		for(i=0; i<self->axes_len; ++i)
			free(self->axes[i]);
		free(self->axes), self->axes = NULL, self->axes_len = 0;
	}

	if(self->holders_len) {
		for(i=0; i<self->holders_len; ++i)
			hkl_holder_release_memory(&self->holders[i]);
		free(self->holders), self->holders = NULL, self->holders_len = 0;
	}

	free(self);
}

HklHolder *hkl_geometry_add_holder(HklGeometry *self)
{
	HklHolder *holder;
	size_t len;

	len = self->holders_len++;
	self->holders = realloc(self->holders, self->holders_len*sizeof(HklHolder));
	holder = &self->holders[len];
	hkl_holder_init(holder, self);

	return holder;
}

void hkl_geometry_update(HklGeometry *self)
{
	size_t i;

	for(i=0; i<self->holders_len; i++)
		hkl_holder_update(&self->holders[i]);

	for(i=0; i<self->axes_len; i++)
		hkl_axis_clear_dirty(self->axes[i]);
}

void hkl_geometry_fprintf(FILE *file, HklGeometry const *self)
{
	size_t i;
	HklAxis const *axis;
	double value;

	for(i=0; i<self->axes_len; ++i) {
		axis = self->axes[i];
		value = axis->config.value;
		value *= HKL_RADTODEG;
		fprintf(file, " %s : %f", axis->name, value);
	}
}
