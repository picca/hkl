#include <math.h>
#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-geometry.h>

/* private part */

static void hkl_holder_update(HklHolder *self)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	size_t i;
	self->q = q0;
	for(i=0; i<self->axes_len; ++i) {
		HklQuaternion q;

		hkl_axis_get_quaternion(self->axes[i], &q);
		hkl_quaternion_times_quaternion(&self->q, &q);
	}
}

/* public part */

HklGeometry *hkl_geometry_new(void)
{
	HklGeometry *g = NULL;

	g = malloc(sizeof(*g));
	if (!g)
		die("Cannot allocate a HklGeometry struct !!!");

	g->name = NULL;
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

	copy->name = src->name;
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

void hkl_geometry_init_geometry(HklGeometry *self, HklGeometry const *src)
{
	/* check if the geometries are compatibles */
	/* compatible is not equal!!! */
	if (self->axes_len == src->axes_len
	    && self->holders_len == src->holders_len) {
		size_t i;

		self->source = src->source;

		// copy the axes configuration and mark it as dirty
		for(i=0; i<src->axes_len; ++i)
			self->axes[i]->config = src->axes[i]->config;
		for(i=0; i<src->holders_len; ++i)
			self->holders[i].q = src->holders[i].q;
	}
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
	int ko = 0;
	for(i=0; i<self->axes_len; ++i)
		if (self->axes[i]->config.dirty == 1) {
			ko = 1;
			break;
		}

	if (ko) {
		for(i=0; i<self->holders_len; i++)
			hkl_holder_update(&self->holders[i]);

		for(i=0; i<self->axes_len; i++)
			self->axes[i]->config.dirty = 0;
	}
}

HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self, char const *name)
{
	size_t i;
	HklAxis *axis;
	for(i=0; i<self->axes_len; ++i) {
		axis = self->axes[i];
		if (!strcmp(axis->name, name))
			return axis;
	}
	return NULL;
}

void hkl_geometry_randomize(HklGeometry *self)
{
	size_t i;

	for(i=0; i<self->axes_len; ++i)
		hkl_axis_config_randomize(&self->axes[i]->config);
	hkl_geometry_update(self);
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
