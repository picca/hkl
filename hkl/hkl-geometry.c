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
#include <alloca.h>

#include <gsl/gsl_math.h>
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

	/* check if an axis with the same name is on the axis list */
	for(i=0; i<HKL_LIST_LEN(geometry->axes); ++i){
		HklAxis *axis;

		axis = &geometry->axes[i];
		if(!strcmp(hkl_axis_get_name(axis), name)){
			if (hkl_vector_cmp(&axis->axis_v, axis_v))
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
				    name,
				    axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
				    axis_v->data[0], axis_v->data[1], axis_v->data[2]);
			else
				return i;
		}
	}

	/* no so create and add it to the list */
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
	/* check axes compatibility */
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
	HklVector axis_v;

	axis_v.data[0] = x;
	axis_v.data[1] = y;
	axis_v.data[2] = z;

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

	g = HKL_MALLOC(HklGeometry);

	g->config = NULL;
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

	self = HKL_MALLOC(HklGeometry);

	self->config = src->config;
	self->source = src->source;

	/* copy the axes */
	HKL_LIST_ALLOC(self->axes, HKL_LIST_LEN(src->axes));
	HKL_LIST_COPY(self->axes, src->axes);

	/* copy the holders */
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

	if(!self || !src
	   || self->config->type != src->config->type)
		return;

	self->source = src->source;

	/* copy the axes configuration and mark it as dirty */
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
		if (hkl_axis_get_changed(&self->axes[i]) == HKL_TRUE) {
			ko = 1;
			break;
		}

	if (ko) {
		for(i=0; i<HKL_LIST_LEN(self->holders); i++)
			hkl_holder_update(&self->holders[i]);

		for(i=0; i<len; i++)
			hkl_axis_set_changed(&self->axes[i], HKL_FALSE);
	}
}

HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self, char const *name)
{
	size_t i;
	HklAxis *axis;
	for(i=0; i<HKL_LIST_LEN(self->axes); ++i) {
		axis = &self->axes[i];
		if (!strcmp(hkl_axis_get_name(axis), name))
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
	double value1, value2;
	double distance = 0.;

	if (!self || !geom)
		return 0.;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i){
		value1 = hkl_axis_get_value(&self->axes[i]);
		value2 = hkl_axis_get_value(&geom->axes[i]);
		distance += fabs(value2 - value1);
	}

	return distance;
}

double hkl_geometry_distance_orthodromic(HklGeometry *self, HklGeometry *geom)
{
	size_t i;
	double value1, value2;
	double distance = 0.;

	if (!self || !geom)
		return 0.;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i){
		double d;

		value1 = hkl_axis_get_value(&self->axes[i]);
		value2 = hkl_axis_get_value(&geom->axes[i]);
		d = fabs(gsl_sf_angle_restrict_symm(value2) - gsl_sf_angle_restrict_symm(value1));
		/* as M_PI and -M_PI are included in the GSL restriction */
		if (d > M_PI)
			d = 2*M_PI - d;
		distance += d;
	}

	return distance;
}

int hkl_geometry_is_valid(const HklGeometry *self)
{
	size_t i;
	size_t len;

	len = HKL_LIST_LEN(self->axes);
	for(i=0; i<len; ++i)
		if(hkl_axis_is_valid(&self->axes[i]) == HKL_FALSE)
			return HKL_FALSE;

	return HKL_TRUE;
}

int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self, HklGeometry *ref)
{
	size_t i;
	size_t len = HKL_LIST_LEN(self->axes);
	double *values = alloca(len * sizeof(*values));
	int ko = HKL_FALSE;

	for(i=0;i<len;++i){
		values[i] = hkl_axis_get_value_closest(&self->axes[i], &ref->axes[i]);
		if(gsl_isnan(values[i])){
			ko = HKL_TRUE;
			break;
		}
	}
	if(!ko){
		for(i=0;i<len;++i)
			hkl_axis_set_value(&self->axes[i], values[i]);
		hkl_geometry_update(self);
	}
	return ko;
}

void hkl_geometry_fprintf(FILE *file, HklGeometry const *self)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(self->axes); ++i){
		if(i)
			fprintf(file, "\n");
		hkl_parameter_fprintf(file, (HklParameter *)(&self->axes[i]));
	}
}

/*******************/
/* HklGeometryList */
/*******************/

HklGeometryList *hkl_geometry_list_new(void)
{
	HklGeometryList *self;

	self = HKL_MALLOC(HklGeometryList);

	HKL_LIST_INIT(self->items);
	self->multiply = NULL;

	return self;
}

void hkl_geometry_list_free(HklGeometryList *self)
{
	HKL_LIST_FREE_DESTRUCTOR(self->items, hkl_geometry_list_item_free);
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
void hkl_geometry_list_add(HklGeometryList *self, HklGeometry *geometry)
{
	size_t i;
	int ko;

	/* now check if the geometry is already in the geometry list */
	ko = HKL_FALSE;
	for(i=0; i<HKL_LIST_LEN(self->items); ++i)
		if (hkl_geometry_distance_orthodromic(geometry,
						      self->items[i]->geometry) < HKL_EPSILON)
			ko = HKL_TRUE;

	if(ko == HKL_FALSE)
		HKL_LIST_ADD_VALUE(self->items, hkl_geometry_list_item_new(geometry));
}

void hkl_geometry_list_reset(HklGeometryList *self)
{
	HKL_LIST_FREE_DESTRUCTOR(self->items, hkl_geometry_list_item_free);
}

void hkl_geometry_list_sort(HklGeometryList *self, HklGeometry *ref)
{
	size_t len = HKL_LIST_LEN(self->items);
	double *distances = alloca(len * sizeof(*distances));
	size_t *idx = alloca(len * sizeof(*idx));
	size_t i, x;
	int j, p;
	HklGeometryListItem **items;

	/* compute the distances once for all */
	for(i=0; i<len; ++i){
		distances[i] = hkl_geometry_distance(ref, self->items[i]->geometry);
		idx[i] = i;
	}

	/* insertion sorting */
	for(i=1; i<len; ++i){
		x = idx[i];
		/* find the smallest idx p lower than i with distance[idx[p]] >= distance[x] */
		for(p = 0; distances[idx[p]] < distances[x] && fabs(distances[idx[p]] - distances[x]) > HKL_EPSILON; p++);
 
		/* move evythings in between p and i */
		for(j=i-1; j>=p; j--)
			idx[j+1] = idx[j];

		idx[p] = x; /* insert the saved idx */
	}

	/* reorder the geometries. */
	items = malloc(len * sizeof(HklGeometryListItem *));
	for(i=0; i<len; ++i)
		items[i] = self->items[idx[i]];
	free(self->items);
	self->items = items;
}

void hkl_geometry_list_fprintf(FILE *f, HklGeometryList const *self)
{
	HklParameter *parameter;
	HklGeometry *g;
	double value;
	size_t len, axes_len;
	size_t i, j;

	fprintf(f, "multiply method: %p \n", self->multiply);
	len = HKL_LIST_LEN(self->items);
	if(len){
		axes_len = HKL_LIST_LEN(self->items[0]->geometry->axes);
		g = self->items[0]->geometry;
		fprintf(f, "    ");
		for(i=0; i<axes_len; ++i)
			fprintf(f, "%19s", hkl_axis_get_name(&g->axes[i]));

		/* geometries */
		for(i=0; i<len; ++i) {
			fprintf(f, "\n%d :", i);
			for(j=0; j<axes_len; ++j) {
				parameter = (HklParameter *)(&self->items[i]->geometry->axes[j]);
				value = hkl_parameter_get_value_unit(parameter);
				if (parameter->punit)
					fprintf(f, " % 18.15f %s", value, parameter->punit->repr);
				else
					fprintf(f, " % 18.15f", value);

			}
			fprintf(f, "\n   ");
			for(j=0; j<axes_len; ++j) {
				parameter = (HklParameter *)(&self->items[i]->geometry->axes[j]);
				value = gsl_sf_angle_restrict_symm(parameter->value) * hkl_unit_factor(parameter->unit, parameter->punit);
				if (parameter->punit)
					fprintf(f, " % 18.15f %s", value, parameter->punit->repr);
				else
					fprintf(f, " % 18.15f", value);
			}
			fprintf(f, "\n");
		}
	}
}

void hkl_geometry_list_multiply(HklGeometryList *self)
{
	size_t i;
	size_t len;

	if(!self || !self->multiply)
		return;

	len = HKL_LIST_LEN(self->items);
	for(i=0; i<len; ++i)
		self->multiply(self, i);
}

static void perm_r(HklGeometryList *self, HklGeometry *ref, HklGeometry *geometry,
		   int perm[], size_t axis_idx)
{
	size_t len;

	len = HKL_LIST_LEN(geometry->axes);

	if (axis_idx == len){
		if(hkl_geometry_distance(ref, geometry) > HKL_EPSILON)
			HKL_LIST_ADD_VALUE(self->items, hkl_geometry_list_item_new(geometry));
	}else{
		if(perm[axis_idx] == HKL_TRUE){
			HklAxis *axis;
			double max;
			double value;
			double value0;

			axis = &geometry->axes[axis_idx];
			max = hkl_axis_get_max(axis);
			value = hkl_axis_get_value(axis);
			value0 = value;
			do{
				/* fprintf(stdout, "\n%d %s, %f", axis_idx, hkl_axis_get_name(axis), value * HKL_RADTODEG); */
				perm_r(self, ref, geometry, perm, axis_idx + 1);
				value +=  2*M_PI;
				if(value <= (max + HKL_EPSILON))
					hkl_axis_set_value(axis, value);
			}while(value <= (max + HKL_EPSILON));
			hkl_axis_set_value(axis, value0);
		} else
			perm_r(self, ref, geometry, perm, axis_idx + 1);
	}	
}

void hkl_geometry_list_multiply_from_range(HklGeometryList *self)
{
	size_t i, j;
	size_t len;
	if(!self)
		return;

	len = HKL_LIST_LEN(self->items);
	for(i=0; i<len; ++i){
		HklGeometry *geometry;
		HklGeometry *ref;
		int *perm;

		ref = self->items[i]->geometry;
		geometry = hkl_geometry_new_copy(ref);
		perm = alloca(HKL_LIST_LEN(geometry->axes) * sizeof(*perm));

		/* find axes to permute and the first solution of thoses axes */
		for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j){
			HklAxis *axis = &geometry->axes[j];
			perm[j] = hkl_axis_is_value_compatible_with_range(axis);
			/* fprintf(stdout, "%d %d\n", j, perm[j]); */
			if (perm[j] == HKL_TRUE)
				hkl_axis_set_value_smallest_in_range(axis);
				
		}
		/*
		 * fprintf(stdout, "FIRST SOLUTION\n");
		 * hkl_geometry_fprintf(stdout, geometry);
		 */

		perm_r(self, ref, geometry, perm, 0);
		hkl_geometry_free(geometry);
	}
}

void hkl_geometry_list_remove_invalid(HklGeometryList *self)
{
	size_t i;
	
	if(!self)
		return;
	
	for(i=0; i<HKL_LIST_LEN(self->items); ++i)
		if(!hkl_geometry_is_valid(self->items[i]->geometry)){
			HKL_LIST_DEL_DESTRUCTOR(self->items, i, hkl_geometry_list_item_free);
			--i;
		}
}

int hkl_geometry_list_len(HklGeometryList *self)
{
	return HKL_LIST_LEN(self->items);
}

int hkl_geometry_list_is_empty(HklGeometryList *self)
{
	return HKL_LIST_LEN(self->items) == 0;
}

/***********************/
/* HklGeometryListItem */
/***********************/

HklGeometryListItem *hkl_geometry_list_item_new(HklGeometry *geometry)
{
	HklGeometryListItem *self;

	if(!geometry)
		return NULL;

	self = HKL_MALLOC(HklGeometryListItem);

	self->geometry = hkl_geometry_new_copy(geometry);

	return self;
}


void hkl_geometry_list_item_free(HklGeometryListItem *self)
{
	if(!self)
		return;

	hkl_geometry_free(self->geometry);
	free(self);
}
