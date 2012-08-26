/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <math.h>
#include <string.h>
#include <stdarg.h>
#ifndef _MSC_VER
# include <alloca.h>
#endif

#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-geometry.h>

/*
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
static size_t hkl_geometry_add_rotation(HklGeometry *self,
					const char *name, const HklVector *axis_v)
{
	size_t i, len;
	HklAxis axis;

	/* check if an axis with the same name is on the axis list */
	for(i=0; i<self->len; ++i){
		HklAxis *axis;

		axis = &self->axes[i];
		if(!strcmp(axis->parameter.name, name)){
			if (hkl_vector_cmp(&axis->axis_v, axis_v)){
				fprintf(stderr, "can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
					name,
					axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
					axis_v->data[0], axis_v->data[1], axis_v->data[2]);
				exit(128);
			}else{
				return i;
			}
		}
	}

	/* no so create and add it to the list */
	self->axes = realloc(self->axes, sizeof(*self->axes) * (self->len + 1));
	hkl_axis_init(&self->axes[self->len], name, axis_v);

	return self->len++;
}

/*******************/
/* HklHolderConfig */
/*******************/

static struct HklHolderConfig *hkl_holder_config_new(void)
{
	struct HklHolderConfig *self;

	self = HKL_MALLOC(struct HklHolderConfig);

	self->gc = 1;
	self->idx = NULL;
	self->len = 0;

	return self;
}

static struct HklHolderConfig *hkl_holder_config_ref(struct HklHolderConfig *self)
{
	if(!self)
		return NULL;

	self->gc++;

	return self;
}

static void hkl_holder_config_unref(struct HklHolderConfig *self)
{
	if(!self)
		return;

	if(--self->gc)
		return;

	free(self->idx);
	free(self);
}

/*************/
/* HklHolder */
/*************/

static void hkl_holder_init(HklHolder *self, HklGeometry *geometry)
{
	static HklQuaternion q0 = {{0, 0, 0, 0}};
	self->config = hkl_holder_config_new();
	self->geometry = geometry;
	self->q = q0;
}

static int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
				const HklHolder *holder)
{
	/* check axes compatibility */
	if (geometry->len != holder->geometry->len)
		return HKL_FALSE;

	self->config = hkl_holder_config_ref(holder->config);
	self->geometry = geometry;
	self->q = holder->q;

	return HKL_TRUE;
}

static void hkl_holder_release_memory(HklHolder *self)
{
	hkl_holder_config_unref(self->config);
}

static void hkl_holder_update(HklHolder *self)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	size_t i;
	HklAxis *axes;
	size_t *idx;

	self->q = q0;
	axes = self->geometry->axes;
	idx = self->config->idx;
	for(i=0; i<self->config->len; ++i)
		hkl_quaternion_times_quaternion(&self->q, &axes[idx[i]].q);
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder *self,
				      const char *name, double x, double y, double z)
{
	HklAxis *axis = NULL;
	size_t i, idx;
	HklVector axis_v;

	axis_v.data[0] = x;
	axis_v.data[1] = y;
	axis_v.data[2] = z;

	idx = hkl_geometry_add_rotation(self->geometry, name, &axis_v);

	/* check that the axis is not already in the holder */
	for(i=0; i<self->config->len; i++)
		if (idx == self->config->idx[i])
			return NULL;

	axis = &self->geometry->axes[idx];
	self->config->idx = realloc(self->config->idx, sizeof(*self->config->idx) * (self->config->len + 1));
	self->config->idx[self->config->len++] = idx;

	return axis;
}

/***************/
/* HklGeometry */
/***************/

/**
 * hkl_geometry_new: (skip)
 *
 * constructor
 *
 * Returns:
 **/
HklGeometry *hkl_geometry_new(void)
{
	HklGeometry *g = NULL;

	g = HKL_MALLOC(HklGeometry);

	g->config = NULL;
	hkl_source_init(&g->source, 1.54, 1, 0, 0);
	g->axes = NULL;
	g->len = 0;
	g->holders = NULL;
	g->holders_len = 0;

	return g;
}

/**
 * hkl_geometry_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklGeometry *hkl_geometry_new_copy(const HklGeometry *self)
{
	HklGeometry *dup = NULL;
	unsigned int i;
	size_t len;

	if(!self)
		return dup;

	dup = HKL_MALLOC(HklGeometry);

	dup->config = self->config;
	dup->source = self->source;
	dup->len = self->len;
	dup->holders_len = self->holders_len;

	/* copy the axes */
	dup->axes = malloc(sizeof(*dup->axes) * dup->len);
	memcpy(dup->axes, self->axes, sizeof(*dup->axes) * dup->len);

	/* copy the holders */
	dup->holders = malloc(sizeof(*dup->holders) * dup->holders_len);
	for(i=0; i<dup->holders_len; ++i)
		hkl_holder_init_copy(&dup->holders[i], dup,
				     &self->holders[i]);

	return dup;
}

/**
 * hkl_geometry_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_geometry_free(HklGeometry *self)
{
	size_t i;

	free(self->axes);
	self->len = 0;

	if(self->holders_len) {
		for(i=0; i<self->holders_len; ++i)
			hkl_holder_release_memory(&self->holders[i]);
		free(self->holders);
	}

	free(self);
}

/**
 * hkl_geometry_init_geometry: (skip)
 * @self:
 * @src:
 *
 * initilize an HklGeometry
 **/
void hkl_geometry_init_geometry(HklGeometry *self, const HklGeometry *src)
{
	size_t i;

	if(!self || !src
	   || self->config->type != src->config->type)
		return;

	self->source = src->source;

	/* copy the axes configuration and mark it as dirty */
	memcpy(self->axes, src->axes, sizeof(*self->axes) * self->len);
	for(i=0; i<src->holders_len; ++i)
		self->holders[i].q = src->holders[i].q;
}

/**
 * hkl_geometry_add_holder: (skip)
 * @self:
 *
 * add an Holder to the #HklGeometry
 *
 * Returns:
 **/
HklHolder *hkl_geometry_add_holder(HklGeometry *self)
{
	self->holders = realloc(self->holders, sizeof(*self->holders) * (self->holders_len + 1));
	hkl_holder_init(&self->holders[self->holders_len], self);

	return &self->holders[self->holders_len++];
}

/**
 * hkl_geometry_update: (skip)
 * @self:
 *
 * update the geometry internal once an Axis values changed
 **/
void hkl_geometry_update(HklGeometry *self)
{
	size_t i;
	int ko = 0;

	for(i=0; i<self->len; ++i)
		if (self->axes[i].parameter.changed) {
			ko = 1;
			break;
		}

	if (ko) {
		for(i=0; i<self->holders_len; i++)
			hkl_holder_update(&self->holders[i]);

		for(i=0; i<self->len; i++)
			self->axes[i].parameter.changed = HKL_FALSE;
	}
}

/**
 * hkl_geometry_get_axis_idx_by_name: (skip)
 * @self:
 * @name:
 *
 * get the index of the axes named @name in the geometry
 *
 * Returns: -1 if the axis was not found
 **/
int hkl_geometry_get_axis_idx_by_name(HklGeometry *self, const char *name)
{
	uint i;
	HklAxis *axis;

	if (!self || !name)
		return -1;

	for(i=0; i<self->len; ++i){
		axis = &self->axes[i];
		if (!strcmp(axis->parameter.name, name))
			return i;
	}

	return -1;
}

/**
 * hkl_geometry_get_axis_by_name:
 * @self:
 * @name:
 *
 * get an #HklAxis using its name
 *
 * Returns: (transfer none):
 **/
HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self, const char *name)
{
	size_t i;
	HklAxis *axis;
	for(i=0; i<self->len; ++i) {
		axis = &self->axes[i];
		if (!strcmp(axis->parameter.name, name))
			return axis;
	}
	return NULL;
}

/**
 * hkl_geometry_randomize: (skip)
 * @self:
 *
 * randomize the #HklGeometry
 **/
void hkl_geometry_randomize(HklGeometry *self)
{
	size_t i;

	for(i=0; i<self->len; ++i)
		hkl_axis_randomize(&self->axes[i]);
	hkl_geometry_update(self);
}

/**
 * hkl_geometry_set_values_v: (skip)
 * @self:
 * @len:
 * "...:
 *
 * set the axes values
 *
 * Returns:
 **/
int hkl_geometry_set_values_v(HklGeometry *self, size_t len, ...)
{
	va_list ap;
	size_t i;

	if (!self || len != self->len)
		return HKL_FALSE;

	va_start(ap, len);
	for(i=0; i<len; ++i)
		hkl_parameter_set_value(&self->axes[i].parameter,
					va_arg(ap, double));

	va_end(ap);
	hkl_geometry_update(self);

	return HKL_TRUE;
}

int hkl_geometry_set_values_unit_v(HklGeometry *self, ...)
{
	va_list ap;
	size_t i;

	if (!self)
		return HKL_FALSE;

	va_start(ap, self);
	for(i=0; i<self->len; ++i)
		hkl_axis_set_value_unit(&self->axes[i], va_arg(ap, double));

	va_end(ap);
	hkl_geometry_update(self);

	return HKL_TRUE;
}

/**
 * hkl_geometry_distance:
 * @self: the this ptr
 * @ref: the #HklGeometry to compare with
 *
 * compute the distance between two #HklGeometries
 *
 * Returns: the distance between the two geometries
 **/
double hkl_geometry_distance(HklGeometry *self, HklGeometry *ref)
{
	size_t i;
	double value1, value2;
	double distance = 0.;

	if (!self || !ref)
		return 0.;

	for(i=0; i<self->len; ++i){
		value1 = hkl_parameter_get_value(&self->axes[i].parameter);
		value2 = hkl_parameter_get_value(&ref->axes[i].parameter);
		distance += fabs(value2 - value1);
	}

	return distance;
}

/**
 * hkl_geometry_distance_orthodromic: (skip)
 * @self: the this ptr
 * @ref: the reference #HklGeometry to compare with.
 *
 * Returns: the orthodromique distance
 **/
double hkl_geometry_distance_orthodromic(HklGeometry *self, HklGeometry *ref)
{
	size_t i;
	double value1, value2;
	double distance = 0.;

	if (!self || !ref)
		return 0.;

	for(i=0; i<self->len; ++i){
		double d;

		value1 = hkl_parameter_get_value(&self->axes[i].parameter);
		value2 = hkl_parameter_get_value(&ref->axes[i].parameter);
		d = fabs(gsl_sf_angle_restrict_symm(value2) - gsl_sf_angle_restrict_symm(value1));
		/* as M_PI and -M_PI are included in the GSL restriction */
		if (d > M_PI)
			d = 2*M_PI - d;
		distance += d;
	}

	return distance;
}

/**
 * hkl_geometry_is_valid: (skip)
 * @self:
 *
 * check if all axes of the #HklGeometry are valid.
 *
 * Returns:
 **/
int hkl_geometry_is_valid(const HklGeometry *self)
{
	size_t i;

	for(i=0; i<self->len; ++i)
		if(hkl_axis_is_valid(&self->axes[i]) == HKL_FALSE)
			return HKL_FALSE;

	return HKL_TRUE;
}

/**
 * hkl_geometry_closest_from_geometry_with_range: (skip)
 * @self:
 * @ref:
 *
 * get the closest axes values in the HklInterval compatible with the
 * current axes values
 *
 * Returns:
 **/
int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self, HklGeometry *ref)
{
	size_t i;
	double *values = alloca(self->len * sizeof(*values));
	int ko = HKL_FALSE;

	for(i=0;i<self->len;++i){
		values[i] = hkl_axis_get_value_closest(&self->axes[i], &ref->axes[i]);
		if(gsl_isnan(values[i])){
			ko = HKL_TRUE;
			break;
		}
	}
	if(!ko){
		for(i=0;i<self->len;++i)
			hkl_parameter_set_value(&self->axes[i].parameter,
						values[i]);
		hkl_geometry_update(self);
	}
	return ko;
}

/**
 * hkl_geometry_fprintf: (skip)
 * @file:
 * @self:
 *
 * print into a file the #HklGeometry
 **/
void hkl_geometry_fprintf(FILE *file, const HklGeometry *self)
{
	size_t i;

	if(!self)
		return;

	for(i=0; i<self->len; ++i){
		if(i)
			fprintf(file, "\n");
		hkl_parameter_fprintf(file, (HklParameter *)(&self->axes[i]));
	}
}

/*******************/
/* HklGeometryList */
/*******************/

/**
 * hkl_geometry_list_new: (skip)
 *
 * constructor
 *
 * Returns:
 **/
HklGeometryList *hkl_geometry_list_new(void)
{
	HklGeometryList *self;

	self = HKL_MALLOC(HklGeometryList);

	list_head_init(&self->items);
	self->len = 0;
	self->multiply = NULL;

	return self;
}

/**
 * hkl_geometry_list_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklGeometryList *hkl_geometry_list_new_copy(const HklGeometryList *self)
{
	HklGeometryList *dup;
	HklGeometryListItem *item;
	int i;

	if (!self)
		return NULL;

	dup = HKL_MALLOC(HklGeometryList);

	*dup = *self;
	list_head_init(&dup->items);

	/* now copy the item arrays */
	list_for_each(&self->items, item, node){
		HklGeometryListItem *dup_item = hkl_geometry_list_item_new_copy(item);
		list_add_tail(&dup->items, &dup_item->node);
	}

	return dup;
}

/**
 * hkl_geometry_list_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_geometry_list_free(HklGeometryList *self)
{
	hkl_geometry_list_reset(self);
	free(self);
}

/**
 * @brief: (skip)
 * @self: The current #HklGeometryList
 * @geometry: the #HklGeometry to add
 *
 * this method Add a geometry to the geometries
 *
 * This method try to be clever by allocating memory only if the
 * current length of the geometries is not large enought. Then it just
 * set the geometry axes and copy it to the right geometries. We do
 * not gives the x len as it is equal to the self->axes_len.
 **/
void hkl_geometry_list_add(HklGeometryList *self, HklGeometry *geometry)
{
	int ko;
	HklGeometryListItem *item;

	/* now check if the geometry is already in the geometry list */
	ko = HKL_FALSE;
	list_for_each(&self->items, item, node)
		if (hkl_geometry_distance_orthodromic(geometry,
						      item->geometry) < HKL_EPSILON)
			ko = HKL_TRUE;

	if(ko == HKL_FALSE){
		item = hkl_geometry_list_item_new(geometry);
		list_add_tail(&self->items, &item->node);
		self->len++;
	}
}

/**
 * hkl_geometry_list_reset: (skip)
 * @self:
 *
 * reset the HklGeometry, in fact it is a sort of clean method remove
 * all the items of the list.
 **/
void hkl_geometry_list_reset(HklGeometryList *self)
{
	HklGeometryListItem *item;
	HklGeometryListItem *next;

	list_for_each_safe(&self->items, item, next, node)
		hkl_geometry_list_item_free(item);

	list_head_init(&self->items);
	self->len = 0;
}

/**
 * hkl_geometry_list_sort: (skip)
 * @self:
 * @ref:
 *
 * sort the #HklGeometryList compare to the distance of the given
 * #HklGeometry
 **/
void hkl_geometry_list_sort(HklGeometryList *self, HklGeometry *ref)
{
	double *distances = alloca(self->len * sizeof(*distances));
	size_t *idx = alloca(self->len * sizeof(*idx));
	int i = 0;
	size_t x;
	int j, p;
	HklGeometryListItem **items;
	HklGeometryListItem *item;
	HklGeometryListItem *next;

	/* prepare a vector to sort the items */
        /* it should be better to sort directly the list instead of
	 * this hack */
	items = malloc(self->len * sizeof(*items));

	/* compute the distances once for all */
	list_for_each(&self->items, item, node){
		distances[i] = hkl_geometry_distance(ref, item->geometry);
		idx[i] = i;
		items[i] = item;
		i++;
	}

	/* insertion sorting */
	for(i=1; i<self->len; ++i){
		x = idx[i];
		/* find the smallest idx p lower than i with distance[idx[p]] >= distance[x] */
		for(p = 0; distances[idx[p]] < distances[x] && fabs(distances[idx[p]] - distances[x]) > HKL_EPSILON; p++);

		/* move everythings in between p and i */
		for(j=i-1; j>=p; j--)
			idx[j+1] = idx[j];

		idx[p] = x; /* insert the saved idx */
	}

	list_head_init(&self->items);
	for(i=0; i<self->len; ++i)
		list_add_tail(&self->items, &items[idx[i]]->node);
	free(items);
}

/**
 * hkl_geometry_list_fprintf: (skip)
 * @f:
 * @self:
 *
 * print to a file the #HklGeometryList
 **/
void hkl_geometry_list_fprintf(FILE *f, const HklGeometryList *self)
{
	HklGeometry *g;
	double value;
	size_t axes_len;
	size_t i, j;

	if(!self)
		return;

	fprintf(f, "multiply method: %p \n", self->multiply);
	if(self->len){
		HklGeometryListItem *item;

		item = list_top(&self->items, HklGeometryListItem, node);
		axes_len = item->geometry->len;
		fprintf(f, "    ");
		for(i=0; i<axes_len; ++i)
			fprintf(f, "%19s", item->geometry->axes[i].parameter.name);

		/* geometries */
		list_for_each(&self->items, item, node){
			HklParameter *parameter;

			fprintf(f, "\n%d :", i);
			for(j=0; j<axes_len; ++j) {
				parameter = (HklParameter *)(&item->geometry->axes[j]);
				value = hkl_parameter_get_value_unit(parameter);
				if (parameter->punit)
					fprintf(f, " % 18.15f %s", value, parameter->punit->repr);
				else
					fprintf(f, " % 18.15f", value);

			}
			fprintf(f, "\n   ");
			for(j=0; j<axes_len; ++j) {
				parameter = &item->geometry->axes[j].parameter;
				value = hkl_parameter_get_value(parameter);
				value = gsl_sf_angle_restrict_symm(value);
				value *= hkl_unit_factor(parameter->unit, parameter->punit);
				if (parameter->punit)
					fprintf(f, " % 18.15f %s", value, parameter->punit->repr);
				else
					fprintf(f, " % 18.15f", value);
			}
			fprintf(f, "\n");
		}
	}
}

/**
 * hkl_geometry_list_multiply: (skip)
 * @self:
 *
 * apply the multiply lenthod to the #HklGeometry
 **/
void hkl_geometry_list_multiply(HklGeometryList *self)
{
	HklGeometryListItem *item;
	HklGeometryListItem *last;

	if(!self || !self->multiply)
		return;

	/*
	 * warning this method change the self->len so we need to save it
	 * before using the recursive perm_r calls
	 */
	last = list_tail(&self->items, HklGeometryListItem, node);
	list_for_each(&self->items, item, node){
		self->multiply(self, item);
		if (item == last)
			break;
	}
}

static void perm_r(HklGeometryList *self, HklGeometry *ref, HklGeometry *geometry,
		   int perm[], size_t axis_idx)
{
	if (axis_idx == geometry->len){
		if(hkl_geometry_distance(ref, geometry) > HKL_EPSILON){
			HklGeometryListItem *item;

			item = hkl_geometry_list_item_new(geometry);
			list_add_tail(&self->items, &item->node);
			self->len++;
		}
	}else{
		if(perm[axis_idx]){
			HklAxis *axis;
			double max;
			double value;
			double value0;

			axis = &geometry->axes[axis_idx];
			max = hkl_parameter_get_max(&axis->parameter);
			value = hkl_parameter_get_value(&axis->parameter);
			value0 = value;
			do{
				/* fprintf(stdout, "\n%d %s, %f", axis_idx, hkl_axis_get_name(axis), value * HKL_RADTODEG); */
				perm_r(self, ref, geometry, perm, axis_idx + 1);
				value +=  2*M_PI;
				if(value <= (max + HKL_EPSILON))
					hkl_parameter_set_value(&axis->parameter, value);
			}while(value <= (max + HKL_EPSILON));
			hkl_parameter_set_value(&axis->parameter, value0);
		} else
			perm_r(self, ref, geometry, perm, axis_idx + 1);
	}
}

void hkl_geometry_list_multiply_from_range(HklGeometryList *self)
{
	size_t j;
	HklGeometryListItem *item;
	HklGeometryListItem *last;

	if(!self)
		return;

	/*
	 * warning this method change the self->len so we need to save it
	 * before using the recursive perm_r calls
	 */
	last = list_tail(&self->items, HklGeometryListItem, node);
	list_for_each(&self->items, item, node){
		HklGeometry *geometry;
		HklGeometry *ref;
		int *perm;

		ref = item->geometry;
		geometry = hkl_geometry_new_copy(ref);
		perm = alloca(geometry->len * sizeof(*perm));

		/* find axes to permute and the first solution of thoses axes */
		for(j=0; j<geometry->len; ++j){
			HklAxis *axis = &geometry->axes[j];
			perm[j] = hkl_axis_is_value_compatible_with_range(axis);
			/* fprintf(stdout, "%d %d\n", j, perm[j]); */
			if (perm[j])
				hkl_axis_set_value_smallest_in_range(axis);
		}
		/*
		 * fprintf(stdout, "FIRST SOLUTION\n");
		 * hkl_geometry_fprintf(stdout, geometry);
		 */

		perm_r(self, ref, geometry, perm, 0);
		hkl_geometry_free(geometry);
		if (item == last)
			break;
	}
}

/**
 * hkl_geometry_list_remove_invalid: (skip)
 * @self:
 *
 * remove all invalid #HklGeometry from the #HklGeometryList
 **/
void hkl_geometry_list_remove_invalid(HklGeometryList *self)
{
	HklGeometryListItem *item;
	HklGeometryListItem *next;

	if(!self)
		return;

	list_for_each_safe(&self->items, item, next, node)
		if(!hkl_geometry_is_valid(item->geometry)){
			list_del(&item->node);
			hkl_geometry_list_item_free(item);
			self->len--;
		}
}

/***********************/
/* HklGeometryListItem */
/***********************/

/**
 * hkl_geometry_list_item_new: (skip)
 * @geometry:
 *
 * constructor
 *
 * Returns:
 **/
HklGeometryListItem *hkl_geometry_list_item_new(HklGeometry *geometry)
{
	HklGeometryListItem *self;

	if(!geometry)
		return NULL;

	self = HKL_MALLOC(HklGeometryListItem);

	self->geometry = hkl_geometry_new_copy(geometry);

	return self;
}

/**
 * hkl_geometry_list_item_new_copy: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklGeometryListItem *hkl_geometry_list_item_new_copy(const HklGeometryListItem *self)
{
	HklGeometryListItem *dup;

	if(!self)
		return NULL;

	dup = HKL_MALLOC(HklGeometryListItem);

	dup->geometry = hkl_geometry_new_copy(self->geometry);

	return dup;
}

/**
 * hkl_geometry_list_item_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_geometry_list_item_free(HklGeometryListItem *self)
{
	if(!self)
		return;

	hkl_geometry_free(self->geometry);
	free(self);
}
