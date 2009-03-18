#include <math.h>
#include <string.h>
#include <stdarg.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-geometry.h>

/*
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
static size_t hkl_geometry_add_rotation(HklGeometry *geometry,
					char const *name, HklVector const *axis_v)
{
	size_t i, len;
	HklAxis axis;

	// check if an axis with the same name is on the axis list
	for(i=0; i<HKL_LIST_LEN(geometry->axes); ++i){
		HklAxis *axis;

		axis = &geometry->axes[i];
		if(!strcmp(axis->parent.name, name)){
			if (hkl_vector_cmp(&axis->axis_v, axis_v))
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
				    name,
				    axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
				    axis_v->data[0], axis_v->data[1], axis_v->data[2]);
			else
				return i;
		}
	}

	// no so create and add it to the list
	len = HKL_LIST_LEN(geometry->axes);
	hkl_axis_init(&axis, name, axis_v);
	HKL_LIST_ADD_VALUE(geometry->axes, axis);

	return len;
}

/*************/
/* HklHolder */
/*************/

static void hkl_holder_init(HklHolder *self, HklGeometry *geometry)
{
	static HklQuaternion q0 = {{0, 0, 0, 0}};
	self->geometry = geometry;
	HKL_LIST_INIT(self->idx);
	self->q = q0;
}

static int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder)
{
	// check axes compatibility
	if (HKL_LIST_LEN(geometry->axes) != HKL_LIST_LEN(holder->geometry->axes))
		return HKL_FAIL;

	self->geometry = geometry;

	HKL_LIST_ALLOC(self->idx, HKL_LIST_LEN(holder->idx));
	HKL_LIST_COPY(self->idx, holder->idx);

	self->q = holder->q;

	return HKL_SUCCESS;
}

static void hkl_holder_release_memory(HklHolder *self)
{
	HKL_LIST_FREE(self->idx);
}

static void hkl_holder_update(HklHolder *self)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	size_t i;
	HklAxis *axes;
	size_t *idx;

	self->q = q0;
	axes = self->geometry->axes;
	idx = self->idx;
	for(i=0; i<HKL_LIST_LEN(self->idx); ++i)
		hkl_quaternion_times_quaternion(&self->q, &axes[idx[i]].q);
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * self,
				      char const * name, double x, double y, double z)
{
	HklAxis *axis = NULL;
	size_t i, idx;
	HklVector axis_v = {{x, y, z}};

	idx = hkl_geometry_add_rotation(self->geometry, name, &axis_v);

	/* check that the axis is not already in the holder */
	for(i=0; i<HKL_LIST_LEN(self->idx); i++)
		if (idx == self->idx[i])
			return NULL;

	axis = &self->geometry->axes[idx];
	HKL_LIST_ADD_VALUE(self->idx, idx);

	return axis;
}


/***************/
/* HklGeometry */
/***************/

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

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void)
{
	HklGeometryList *self;

	self = malloc(sizeof(*self));
	if (!self)
		die("Cannot allocate a HklGeometryList struct !!!");

	HKL_LIST_INIT(self->geometries);
	self->len = 0;

	return self;
}

extern void hkl_geometry_list_free(HklGeometryList *self)
{
	HKL_LIST_FREE_DESTRUCTOR(self->geometries, hkl_geometry_free);
	self->len = 0;
	free(self);
}

/**
 * @brief this method Add a geometry to the geometries
 *
 * @param self The current PseudoAxeEngine
 * @param x A vector of double with the axes values to put in the geometry.
 *
 * This method try to be clever by allocating memory only if the current
 * length of the geometries is not large enought. Then it just set the
 * geometry axes and copy it to the right geometries. We do not gives the
 * x len as it is equal to the self->axes_len.
 */
void hkl_geometry_list_add(HklGeometryList *self,
			   HklGeometry *geometry)
{
	size_t i;
	int ko;

	/* now check if the geometry is already in the geometry list */
	ko = HKL_FALSE;
	for(i=0; i<self->len; ++i)
		if (hkl_geometry_distance(geometry, self->geometries[i]) < HKL_EPSILON)
			ko = HKL_TRUE;

	if(ko == HKL_FALSE)
		HKL_LIST_ADD_VALUE(self->geometries, hkl_geometry_new_copy(geometry));
}

extern void hkl_geometry_list_reset(HklGeometryList *self)
{
	HKL_LIST_FREE_DESTRUCTOR(self->geometries, hkl_geometry_free);
	self->len = 0;
}
