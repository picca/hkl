#include <math.h>
#include <string.h>
#include <stdarg.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-geometry.h>

/* private part */

static void hkl_holder_update(HklHolder *self)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	size_t i;
	self->q = q0;
	HklAxis *axes = self->geometry->axes;
	size_t *idx = self->idx;
	for(i=0; i<HKL_LIST_LEN(self->idx); ++i)
		hkl_quaternion_times_quaternion(&self->q, &axes[idx[i]].q);
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
	HKL_LIST_INIT(g->axes);
	HKL_LIST_INIT(g->holders);

	return g;
}

HklGeometry *hkl_geometry_new_copy(HklGeometry const *src)
{
	HklGeometry *self = NULL;
	unsigned int i;
	size_t len;

	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate a HklGeometry struct !!!");

	self->name = src->name;
	self->source = src->source;

	// copy the axes
	HKL_LIST_ALLOC(self->axes, HKL_LIST_LEN(src->axes));
	HKL_LIST_COPY(self->axes, src->axes);

	// copy the holders
	len = HKL_LIST_LEN(src->holders);
	HKL_LIST_ALLOC(self->holders, len);
	for(i=0; i<len; ++i)
		hkl_holder_init_copy(&self->holders[i], self,
				     &src->holders[i]);

	return self;
}

void hkl_geometry_free(HklGeometry *self)
{
	size_t i;
	size_t len;

	HKL_LIST_FREE(self->axes);

	len = HKL_LIST_LEN(self->holders);
	if(len) {
		for(i=0; i<len; ++i)
			hkl_holder_release_memory(&self->holders[i]);
		HKL_LIST_FREE(self->holders);
	}

	free(self);
}

void hkl_geometry_init_geometry(HklGeometry *self, HklGeometry const *src)
{
	size_t i;

	self->source = src->source;

	// copy the axes configuration and mark it as dirty
	HKL_LIST_COPY(self->axes, src->axes);
	for(i=0; i<HKL_LIST_LEN(src->holders); ++i)
		self->holders[i].q = src->holders[i].q;
}

HklHolder *hkl_geometry_add_holder(HklGeometry *self)
{
	HklHolder *holder;
	size_t len;

	len = HKL_LIST_LEN(self->holders);
	HKL_LIST_RESIZE(self->holders, len + 1);
	holder = &self->holders[len];
	hkl_holder_init(holder, self);

	return holder;
}

void hkl_geometry_update(HklGeometry *self)
{
	size_t i, len;
	int ko = 0;

	len = HKL_LIST_LEN(self->axes);
	for(i=0; i<len; ++i)
		if (self->axes[i].parent.changed == HKL_TRUE) {
			ko = 1;
			break;
		}

	if (ko) {
		for(i=0; i<HKL_LIST_LEN(self->holders); i++)
			hkl_holder_update(&self->holders[i]);

		for(i=0; i<len; i++)
			(self->axes[i].parent.changed = HKL_FALSE);
	}
}

HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self, char const *name)
{
	size_t i;
	HklAxis *axis;
	for(i=0; i<HKL_LIST_LEN(self->axes); ++i) {
		axis = &self->axes[i];
		if (!strcmp(axis->parent.name, name))
			return axis;
	}
	return NULL;
}

void hkl_geometry_randomize(HklGeometry *self)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i)
		hkl_axis_randomize(&self->axes[i]);
	hkl_geometry_update(self);
}

int hkl_geometry_set_values_v(HklGeometry *self, size_t len, ...)
{
	va_list ap;
	size_t i;

	if (!self || len != HKL_LIST_LEN(self->axes))
		return HKL_FAIL;

	va_start(ap, len);
	for(i=0; i<len; ++i)
		hkl_axis_set_value(&self->axes[i], va_arg(ap, double));

	va_end(ap);
	hkl_geometry_update(self);

	return HKL_SUCCESS;
}

double hkl_geometry_distance(HklGeometry *self, HklGeometry *geom)
{
	size_t i;
	HklParameter *axis1, *axis2;
	double distance = 0.;

	if (!self || !geom)
		return 0.;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i){
		axis1 = (HklParameter *)(&self->axes[i]);
		axis2 = (HklParameter *)(&geom->axes[i]);
		distance += fabs(axis2->value - axis1->value);
	}

	return distance;
}

void hkl_geometry_fprintf(FILE *file, HklGeometry const *self)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i)
		hkl_parameter_fprintf(file, (HklParameter *)(&self->axes[i]));
}
