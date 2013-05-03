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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
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

#include "hkl-geometry-private.h"

/*
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
static size_t hkl_geometry_add_rotation(HklGeometry *self,
					const char *name, const HklVector *axis_v)
{
	uint i = 0;
	HklAxis **axis;

	/* check if an axis with the same name is on the axis list */
	darray_foreach(axis, self->axes){
		if(!strcmp((*axis)->parameter.name, name)){
			if (hkl_vector_cmp(&(*axis)->axis_v, axis_v)){
				fprintf(stderr, "can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
					name,
					(*axis)->axis_v.data[0], (*axis)->axis_v.data[1], (*axis)->axis_v.data[2],
					axis_v->data[0], axis_v->data[1], axis_v->data[2]);
				exit(128);
			}else{
				return i;
			}
		}
		++i;
	}

	/* no so create and add it to the list */
	darray_append(self->axes, hkl_axis_new(name, axis_v));

	return darray_size(self->axes) - 1;
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

static HklHolder *hkl_holder_new(HklGeometry *geometry)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	HklHolder *self = HKL_MALLOC(HklHolder);

	self->config = hkl_holder_config_new();
	self->geometry = geometry;
	self->q = q0;

	return self;
}

static HklHolder *hkl_holder_new_copy(HklHolder *src, HklGeometry *geometry)
{
	HklHolder *self = HKL_MALLOC(HklHolder);

	self->config = hkl_holder_config_ref(src->config);
	self->geometry = geometry;
	self->q = src->q;

	return self;
}

static void hkl_holder_free(HklHolder *self)
{
	hkl_holder_config_unref(self->config);
	free(self);
}

static void hkl_holder_update(HklHolder *self)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};
	size_t i;

	self->q = q0;
	for(i=0; i<self->config->len; ++i)
		hkl_quaternion_times_quaternion(&self->q,
						&darray_item(self->geometry->axes,
							     self->config->idx[i])->q);
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

	axis = darray_item(self->geometry->axes, idx);
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
HklGeometry *hkl_geometry_new(const HklFactory *factory)
{
	HklGeometry *g = NULL;

	g = HKL_MALLOC(HklGeometry);

	g->factory = factory;
	hkl_source_init(&g->source, 1.54, 1, 0, 0);
	darray_init(g->axes);
	darray_init(g->holders);

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
HklGeometry *hkl_geometry_new_copy(const HklGeometry *src)
{
	HklGeometry *self = NULL;
	HklAxis **axis;
	HklHolder **holder;

	if(!src)
		return self;

	self = HKL_MALLOC(HklGeometry);

	self->factory = src->factory;
	self->source = src->source;

	/* copy the axes */
	darray_init(self->axes);
	darray_foreach(axis, src->axes){
		darray_append(self->axes, hkl_axis_new_copy(*axis));
	}


	/* copy the holders */
	darray_init(self->holders);
	darray_foreach(holder, src->holders){
		darray_append(self->holders,
			      hkl_holder_new_copy(*holder, self));
	}

	return self;
}

/**
 * hkl_geometry_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_geometry_free(HklGeometry *self)
{
	HklAxis **axis;
	HklHolder **holder;

	darray_foreach(axis, self->axes){
		hkl_axis_free(*axis);
	}
	darray_free(self->axes);

	darray_foreach(holder, self->holders){
		hkl_holder_free(*holder);
	}
	darray_free(self->holders);

	free(self);
}

void hkl_geometry_set(HklGeometry *self, const HklGeometry *src)
{
	size_t i;

	if(self->factory != src->factory)
		return;

	self->factory = src->factory;
	self->source = src->source;

	/* copy the axes configuration and mark it as dirty */
	for(i=0; i<darray_size(self->axes); ++i)
		hkl_axis_axis_set(darray_item(self->axes, i),
				  darray_item(src->axes, i));

	for(i=0; i<darray_size(src->holders); ++i)
		darray_item(self->holders, i)->q = darray_item(src->holders, i)->q;
}

const darray_axis *hkl_geometry_axes_get(const HklGeometry *self)
{
	return &self->axes;
}

void hkl_geometry_axis_set(HklGeometry *self, const HklAxis *axis)
{
	HklAxis **_axis;

	darray_foreach(_axis, self->axes){
		if (*_axis == axis)
			break;
		if (!strcmp(axis->parameter.name, (*_axis)->parameter.name))
			hkl_axis_axis_set(*_axis, axis);
	}
	hkl_geometry_update(self);
}

double hkl_geometry_wavelength_get(const HklGeometry *self)
{
	return self->source.wave_length;
}

void hkl_geometry_wavelength_set(HklGeometry *self, double wavelength)
{
	self->source.wave_length = wavelength;
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
	hkl_geometry_set(self, src);
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
	HklHolder *holder = hkl_holder_new(self);
	darray_append(self->holders, holder);

	return holder;
}

/**
 * hkl_geometry_update: (skip)
 * @self:
 *
 * update the geometry internal once an Axis values changed
 **/
void hkl_geometry_update(HklGeometry *self)
{
	HklAxis **axis;
	size_t i;
	int ko = 0;

	darray_foreach(axis, self->axes){
		if ((*axis)->parameter.changed) {
			ko = 1;
			break;
		}
	}

	if (ko) {
		HklHolder **holder;

		darray_foreach(holder, self->holders){
			hkl_holder_update(*holder);
		}

		darray_foreach(axis, self->axes){
			(*axis)->parameter.changed = HKL_FALSE;
		}
	}
}

const char *hkl_geometry_name_get(const HklGeometry *self)
{
	return hkl_factory_name(self->factory);
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
int hkl_geometry_get_axis_idx_by_name(const HklGeometry *self, const char *name)
{
	uint i = 0;
	HklAxis **axis;

	if (!self || !name)
		return -1;

	darray_foreach(axis, self->axes){
		if (!strcmp((*axis)->parameter.name, name))
			return i;
		++i;
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
HklParameter *hkl_geometry_get_axis_by_name(HklGeometry *self, const char *name)
{
	HklAxis **axis;

	darray_foreach(axis, self->axes){
		if (!strcmp((*axis)->parameter.name, name))
			return &(*axis)->parameter;
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
	HklAxis **axis;

	darray_foreach(axis, self->axes){
		hkl_parameter_randomize(&(*axis)->parameter);
	}
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
	HklAxis **axis;

	if (!self || darray_size(self->axes) != len)
		return HKL_FALSE;

	va_start(ap, len);
	darray_foreach(axis, self->axes){
		hkl_parameter_set_value(&(*axis)->parameter,
					va_arg(ap, double), NULL);
	}
	va_end(ap);

	hkl_geometry_update(self);

	return HKL_TRUE;
}

int hkl_geometry_set_values_unit_v(HklGeometry *self, ...)
{
	va_list ap;
	HklAxis **axis;

	va_start(ap, self);
	darray_foreach(axis, self->axes){
		hkl_parameter_set_value_unit(&(*axis)->parameter,
					     va_arg(ap, double), NULL);
	}
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
double hkl_geometry_distance(const HklGeometry *self,
			     const HklGeometry *ref)
{
	size_t i;
	double value1, value2;
	double distance = 0.;

	if (!self || !ref)
		return 0.;

	for(i=0; i<darray_size(self->axes); ++i){
		value1 = darray_item(self->axes, i)->parameter._value;
		value2 = darray_item(ref->axes, i)->parameter._value;
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
double hkl_geometry_distance_orthodromic(const HklGeometry *self,
					 const HklGeometry *ref)
{
	size_t i;
	double value1, value2;
	double distance = 0.;

	if (!self || !ref)
		return 0.;

	for(i=0; i<darray_size(self->axes); ++i){
		double d;

		value1 = darray_item(self->axes, i)->parameter._value;
		value2 = darray_item(ref->axes, i)->parameter._value;
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
	HklAxis **axis;

	darray_foreach(axis, self->axes){
		if(!hkl_parameter_is_valid(&(*axis)->parameter))
			return HKL_FALSE;
	}

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
int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self,
						  const HklGeometry *ref)
{
	size_t i;
	uint len = darray_size(self->axes);
	double *values = alloca(len * sizeof(*values));
	int ko = HKL_FALSE;

	for(i=0;i<len;++i){
		values[i] = hkl_parameter_get_value_closest(&darray_item(self->axes, i)->parameter,
							    &darray_item(ref->axes, i)->parameter);
		if(gsl_isnan(values[i])){
			ko = HKL_TRUE;
			break;
		}
	}
	if(!ko){
		for(i=0;i<len;++i)
			hkl_parameter_set_value(&darray_item(self->axes, i)->parameter,
						values[i], NULL);
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
	uint i;

	for(i=0; i<darray_size(self->axes); ++i){
		if(i)
			fprintf(file, "\n");
		hkl_parameter_fprintf(file, &darray_item(self->axes, i)->parameter);
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

	darray_init(self->items);
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
	HklGeometryListItem **item;

	if (!self)
		return NULL;

	dup = HKL_MALLOC(HklGeometryList);

	darray_init(dup->items);
	/* now copy the item arrays */
	darray_foreach(item , self->items){
		darray_append(dup->items, hkl_geometry_list_item_new_copy(*item));
	}
	dup->multiply = self->multiply;

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
	HklGeometryListItem **item;

	/* now check if the geometry is already in the geometry list */
	darray_foreach(item, self->items){
		if (hkl_geometry_distance_orthodromic(geometry,
						      (*item)->geometry) < HKL_EPSILON)
			return;
	}

	darray_append(self->items, hkl_geometry_list_item_new(geometry));
}

const darray_item *hkl_geometry_list_items_get(const HklGeometryList *self)
{
	return &self->items;
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
	HklGeometryListItem **item;

	darray_foreach(item, self->items)
		hkl_geometry_list_item_free(*item);

	darray_free(self->items);
	darray_init(self->items);
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
	double *distances = alloca(darray_size(self->items) * sizeof(*distances));
	size_t *idx = alloca(darray_size(self->items) * sizeof(*idx));
	HklGeometryListItem **items = alloca(darray_size(self->items) * sizeof(*items));
	HklGeometryListItem **item;
	int i = 0;
	size_t x;
	int j, p;

	memcpy(items, &darray_item(self->items, 0), darray_size(self->items) * sizeof(*items));

	/* compute the distances once for all */
	darray_foreach(item, self->items){
		distances[i] = hkl_geometry_distance(ref, (*item)->geometry);
		idx[i] = i;
		i++;
	}

	/* insertion sorting */
	for(i=1; i<darray_size(self->items); ++i){
		x = idx[i];
		/* find the smallest idx p lower than i with distance[idx[p]] >= distance[x] */
		for(p = 0; distances[idx[p]] < distances[x] && fabs(distances[idx[p]] - distances[x]) > HKL_EPSILON; p++);

		/* move everythings in between p and i */
		for(j=i-1; j>=p; j--)
			idx[j+1] = idx[j];

		idx[p] = x; /* insert the saved idx */
	}

	for(i=0; i<darray_size(self->items); ++i){
		darray_item(self->items, i) = items[idx[i]];
	}
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
	uint i = 0;
	double value;

	if(!self)
		return;

	fprintf(f, "multiply method: %p \n", self->multiply);
	if(darray_size(self->items)){
		HklGeometryListItem **item;
		HklAxis **axis;

		fprintf(f, "    ");
		darray_foreach(axis, darray_item(self->items, 0)->geometry->axes){
			fprintf(f, "%19s", (*axis)->parameter.name);
		}

		/* geometries */
		darray_foreach(item, self->items){
			fprintf(f, "\n%d :", i++);
			darray_foreach(axis, (*item)->geometry->axes){
				value = hkl_parameter_get_value_unit(&(*axis)->parameter);
				if ((*axis)->parameter.punit)
					fprintf(f, " % 18.15f %s", value, (*axis)->parameter.punit->repr);
				else
					fprintf(f, " % 18.15f", value);

			}
			fprintf(f, "\n   ");
			darray_foreach(axis, (*item)->geometry->axes){
				value = hkl_parameter_get_value(&(*axis)->parameter);
				value = gsl_sf_angle_restrict_symm(value);
				value *= hkl_unit_factor((*axis)->parameter.unit,
							 (*axis)->parameter.punit);
				if ((*axis)->parameter.punit)
					fprintf(f, " % 18.15f %s", value, (*axis)->parameter.punit->repr);
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
	uint i;
	uint len = darray_size(self->items);

	if(!self || !self->multiply)
		return;

	/*
	 * warning this method change the self->len so we need to save it
	 * before using the recursive perm_r calls
	 */
	for(i=0; i<len; ++i)
		self->multiply(self, darray_item(self->items, i));
}

static void perm_r(HklGeometryList *self, const HklGeometry *ref,
		   const HklGeometry *geometry, const int perm[],
		   const unsigned int axis_idx)
{
	if (axis_idx == darray_size(geometry->axes)){
		if(hkl_geometry_distance(geometry, ref) > HKL_EPSILON)
			darray_append(self->items, hkl_geometry_list_item_new(geometry));
	}else{
		if(perm[axis_idx]){
			HklAxis *axis = darray_item(geometry->axes, axis_idx);
			const double max = axis->parameter.range.max;;
			const double value0 = axis->parameter._value;
			double value;

			value = value0;
			do{
				/* fprintf(stdout, "\n%d %s, %f", axis_idx, hkl_axis_get_name(axis), value * HKL_RADTODEG); */
				perm_r(self, ref, geometry, perm, axis_idx + 1);
				value +=  2*M_PI;
				if(value <= (max + HKL_EPSILON)){
					/* optimisation here: */
					/* instead of using set_value
					 * we directly write the
					 * HklParameter value, BEWARE
					 * that it require that
					 * HklParameter is a rotation
					 * (for now it is always
					 * true */
					axis->parameter._value = value;
				}
			}while(value <= (max + HKL_EPSILON));
			/* restore the initial value */
			axis->parameter._value = value0;
		} else
			perm_r(self, ref, geometry, perm, axis_idx + 1);
	}
}

void hkl_geometry_list_multiply_from_range(HklGeometryList *self)
{
	uint i = 0;
	uint len = darray_size(self->items);
	size_t j = 0;

	if(!self)
		return;

	/*
	 * warning this method change the self->len so we need to save it
	 * before using the recursive perm_r calls
	 */
	for(i=0; i<len; ++i){
		HklGeometry *geometry;
		HklAxis **axis;
		const HklGeometry *ref = darray_item(self->items, i)->geometry;
		int *perm;

		geometry = hkl_geometry_new_copy(ref);
		perm = alloca(darray_size(geometry->axes) * sizeof(*perm));

		/* find axes to permute and the first solution of thoses axes */
		darray_foreach(axis, geometry->axes){
			perm[j] = hkl_parameter_is_valid(&(*axis)->parameter);
			/* fprintf(stdout, "%d %d\n", j, perm[j]); */
			if (perm[j])
				hkl_parameter_set_value_smallest_in_range(&(*axis)->parameter);
			++j;
		}
		/*
		 * fprintf(stdout, "FIRST SOLUTION\n");
		 * hkl_geometry_fprintf(stdout, geometry);
		 */

		perm_r(self, ref, geometry, perm, 0);
		hkl_geometry_free(geometry);
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
	uint len = darray_size(self->items);
	HklGeometryListItem **items = alloca(len * sizeof(*items));
	uint i = 0;

	if(!self)
		return;

	memcpy(items, &darray_item(self->items, 0), len * sizeof(*items));
	darray_size(self->items) = 0;
	for(i=0; i<len; ++i){
		if(!hkl_geometry_is_valid(items[i]->geometry))
			hkl_geometry_list_item_free(items[i]);
		else
			darray_append(self->items, items[i]);
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
HklGeometryListItem *hkl_geometry_list_item_new(const HklGeometry *geometry)
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

/**
 * hkl_geometry_list_item_geometry_get: (skip)
 * @self: the this ptr
 *
 * Return value: The geometry contain inside the HklGeometryListItem
 **/
const HklGeometry *hkl_geometry_list_item_geometry_get(const HklGeometryListItem *self)
{
	return self->geometry;
}
