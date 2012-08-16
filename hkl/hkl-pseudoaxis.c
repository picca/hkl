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
#include <string.h>
#ifndef _MSC_VER
# include <alloca.h>
#endif
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

/*****************/
/* HklPseudoAxis */
/*****************/

/**
 * hkl_pseudo_axis_new: (skip)
 * @parameter:
 * @engine:
 *
 * default constructor
 *
 * Returns: a new HklPseudoAxis
 **/
HklPseudoAxis *hkl_pseudo_axis_new(const HklParameter *parameter,
				   HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *self;

	self = HKL_MALLOC(HklPseudoAxis);

	hkl_pseudo_axis_init(self, parameter, engine);

	return self;
}

/**
 * hkl_pseudo_axis_dup: (skip)
 * @self:
 *
 * copy constructor
 *
 * Returns:
 **/
HklPseudoAxis *hkl_pseudo_axis_dup(const HklPseudoAxis *self)
{
	HklPseudoAxis *dup;

	if(!self)
		return NULL;

	dup = HKL_MALLOC(HklPseudoAxis);

	dup->parent = self->parent;
	dup->engine = self->engine;

	return dup;
}

/**
 * hkl_pseudo_axis_init: (skip)
 * @self:
 * @parameter:
 * @engine:
 *
 * initialize a PseudoAxis with the given parameters
 **/
void hkl_pseudo_axis_init(HklPseudoAxis *self,
			  const HklParameter *parameter,
			  HklPseudoAxisEngine *engine)
{
	self->parent = *parameter;
	self->engine = engine;
}

/**
 * hkl_pseudo_axis_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_pseudo_axis_free(HklPseudoAxis *self)
{
	if(self)
		free(self);
}

/**
 * hkl_pseudo_axis_fprintf: (skip)
 * @f:
 * @self:
 *
 * print an HklPseudoAxis into a file
 **/
void hkl_pseudo_axis_fprintf(FILE *f, HklPseudoAxis *self)
{
	if(!self)
		return;

	hkl_parameter_fprintf(f, &self->parent);
	fprintf(f, " %p", self->engine);
}

/*****************************/
/* HklPseudoAxisEngineMode */
/*****************************/

/**
 * hkl_pseudo_axis_engine_mode_new: (skip)
 * @name: The name of this HklPseudoAxisEngineMode.
 * @op: the HklPseudoAxisEngineModeoperations
 * @n: the number of parameters
 * @...: rest of the parameters
 *
 * This method create an HklPseudoAxisEngineMode structure
 * to be used in a HklPseudoAxisEngine.
 * The variable number of parameters contain in fact n
 * pointers on the parameters the an interger giving the
 * number of related axes and finally the name of thoses
 * related axes.
 *
 * mode = hkl_pseudo_axis_engine_mode_new(
 *	"constant_omega",
 *      hkl_mode_operations,
 *	1, &parameter,
 *	4, "komega", "kappa", "kphi", "tth");
 *
 * Returns:
 **/
HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_new(
	const HklPseudoAxisEngineModeInfo *info,
	const HklPseudoAxisEngineModeOperations *op,
	size_t n_p, ...)
{
	HklPseudoAxisEngineMode *self = NULL;
	va_list ap;
	size_t i;
	HklParameter *parameters;

	/* extract the variable part of the method */

	va_start(ap, n_p);

	/* parameters */
	parameters = alloca(n_p * sizeof(*parameters));
	for(i=0; i<n_p; ++i)
		parameters[i] = va_arg(ap, HklParameter);

	va_end(ap);

	self = HKL_MALLOC(HklPseudoAxisEngineMode);

	self->parameters = NULL;
	hkl_pseudo_axis_engine_mode_init(self, info,
					 op,
					 n_p, parameters);

	return self;
}

/**
 * hkl_pseudo_axis_engine_mode_init: (skip)
 * @self: the HklPseudoAxisEngineMode to initialize
 * @name: The name of this HklPseudoAxisEngineMode
 * @op: the HklPseudoAxisEngineModeOperations
 * @functions_len: the number of HklFunction
 * @functions: the HklFunction array
 * @parameters_len: the number of parameters
 * @parameters: the HklParameter array
 * @axes_len: the length of the axes names
 * @axes: an array with the axes names.
 *
 * This method create an HklPseudoAxisEngineMode structure
 * to be used in a HklPseudoAxisEngine.
 * The variable number of parameters contain in fact n
 * pointers on the parameters the an interger giving the
 * number of related axes and finally the name of thoses
 * related axes.
 *
 * mode = hkl_pseudo_axis_engine_mode_new(
 *	"constant_omega",
 *      mode_operations,
 *      1, functions,
 *	1, &parameter,
 *	4, "komega", "kappa", "kphi", "tth");
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_mode_init(
	HklPseudoAxisEngineMode *self,
	const HklPseudoAxisEngineModeInfo *info,
	const HklPseudoAxisEngineModeOperations *op,
	size_t parameters_len, HklParameter parameters[])
{
	size_t i;

	/* ensure part */
	if (!self)
		return HKL_FALSE;

	self->info = info;
	self->op = op;

	/* parameters */
	self->parameters = realloc(self->parameters, sizeof(*self->parameters) * parameters_len);
	self->parameters_len = parameters_len;
	memcpy(self->parameters, parameters, sizeof(*self->parameters) * parameters_len);

	/* init part */
	self->geometry_init = NULL;
	self->detector_init = NULL;
	self->sample_init = NULL;

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_mode_free: (skip)
 * @self:
 *
 * delete an HklPseudoAxisEngineMode
 **/
void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self)
{
	free(self->parameters);
	self->parameters_len = 0;

	if(self->geometry_init){
		hkl_geometry_free(self->geometry_init);
		self->geometry_init = NULL;
	}
	if(self->detector_init){
		hkl_detector_free(self->detector_init);
		self->detector_init = NULL;
	}
	if(self->sample_init){
		hkl_sample_free(self->sample_init);
		self->sample_init = NULL;
	}
	free(self);
}

/**
 * hkl_pseudo_axis_engine_mode_fprintf: (skip)
 * @f:
 * @self:
 *
 * print to a FILE the HklPSeudoAxisEngineMode members
 **/
void hkl_pseudo_axis_engine_mode_fprintf(FILE *f, const HklPseudoAxisEngineMode *self)
{
	unsigned int i;

	fprintf(f, "mode: \"%s\"\n", self->info->name);
	fprintf(f, "initialize: %p\n", self->op->init);
	fprintf(f, "get: %p\n", self->op->get);
	fprintf(f, "set: %p\n", self->op->set);
	if(self->info->functions){
		fprintf(f, "functions:");
		for(i=0; i<self->info->n_functions; ++i)
			fprintf(f, " %p", self->info->functions[i]);
		fprintf(f, "\n");
	}
	if(self->parameters)
		for(i=0; i<self->parameters_len; ++i){
			hkl_parameter_fprintf(f, &self->parameters[i]);
			fprintf(f, "\n");
		}
	if(self->info->axes){
		fprintf(f, "axes names:");
		for(i=0; i<self->info->n_axes; ++i)
			fprintf(f, " %s", self->info->axes[i]);
		fprintf(f, "\n");
	}
	hkl_geometry_fprintf(f, self->geometry_init);
	hkl_detector_fprintf(f, self->detector_init);
	hkl_sample_fprintf(f, self->sample_init);
}

/***********************/
/* HklPseudoAxisEngine */
/***********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(const HklPseudoAxisEngineInfo *info)
{
	size_t i;
	HklPseudoAxisEngine *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngine);

	self->info = info;
	list_head_init(&self->modes);

	/* create the pseudoAxes */
	list_head_init(&self->pseudo_axes);
	for(i=0; i<self->info->n_pseudo_axes; ++i){
		HklPseudoAxis *pseudo_axis;

		pseudo_axis =  hkl_pseudo_axis_new(&info->pseudo_axes[i]->parent, self);
		list_add_tail(&self->pseudo_axes, &pseudo_axis->list);
	}

	return self;
}

/**
 * hkl_pseudo_axis_engine_free: (skip)
 * @self: the engine to release
 *
 * release the memory of an HklPseudoAxisEngine
 **/
void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self)
{
	size_t i;
	HklPseudoAxisEngineMode *mode;
	HklPseudoAxisEngineMode *next;
	HklPseudoAxis *pseudo_axis;
	HklPseudoAxis *pseudo_axis_next;

	if (self->geometry)
		hkl_geometry_free(self->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);

	if(self->sample)
		hkl_sample_free(self->sample);

	/* release the mode added */
	list_for_each_safe(&self->modes, mode, next, list){
		list_del(&mode->list);
		hkl_pseudo_axis_engine_mode_free(mode);
	}
	self->mode = NULL;

	/* release the HklPseudoAxe memory */
	list_for_each_safe(&self->pseudo_axes, pseudo_axis, pseudo_axis_next, list){
		list_del(&pseudo_axis->list);
		hkl_pseudo_axis_free(pseudo_axis);
	}

	free(self);
}

/**
 * hkl_pseudo_axis_engine_add_mode: (skip)
 * @self:
 * @mode: the mode to add
 *
 * add an HklPseudoAxisEngineMode to the self HklPseudoAxisEngine
 **/
void hkl_pseudo_axis_engine_add_mode(HklPseudoAxisEngine *self,
				     HklPseudoAxisEngineMode *mode)
{
	list_add_tail(&self->modes, &mode->list);
}

/**
 * hkl_pseudo_axis_engine_add_geometry: (skip)
 * @self: the current PseudoAxeEngine
 * @x: x A vector of double with the axes values to put in the geometry.
 *
 * This method try to be clever by allocating memory only if the
 * current length of the geometries is not large enought. Then it just
 * set the geometry axes and copy it to the right geometries. We do
 * not gives the x len as it is equal to the self->axes_len.
 *
 **/
void hkl_pseudo_axis_engine_add_geometry(HklPseudoAxisEngine *self,
					 double const x[])
{
	HklAxis *axis;
	uint i = 0;

	/* copy the axes configuration into the engine->geometry */
	list_for_each(&self->axes, axis, engine_list)
		hkl_axis_set_value(axis, gsl_sf_angle_restrict_symm(x[i++]));

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}

static void hkl_pseudo_axis_engine_prepare_internal(HklPseudoAxisEngine *self)
{
	uint i;

	if(!self || !self->engines)
		return;

	/* set */
	if(self->geometry)
		hkl_geometry_init_geometry(self->geometry, self->engines->geometry);
	else
		self->geometry = hkl_geometry_new_copy(self->engines->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);
	self->detector = hkl_detector_new_copy(self->engines->detector);

	if(self->sample)
		hkl_sample_free(self->sample);
	self->sample = hkl_sample_new_copy(self->engines->sample);

	/* fill the axes member from the function */
	if(self->mode){
		list_head_init(&self->axes);
		for(i=0; i<self->mode->info->n_axes; ++i){
			HklAxis *axis;

			axis = hkl_geometry_get_axis_by_name(self->geometry,
							     self->mode->info->axes[i]);

			list_add_tail(&self->axes, &axis->engine_list);
		}
	}

	/* reset the geometries len */
	hkl_geometry_list_reset(self->engines->geometries);
}

/**
 * hkl_pseudo_axis_engine_select_mode: (skip)
 * @self: the HklPseudoAxisEngine
 * @idx: the index of the mode you want to select
 *
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes. this method
 * is usually only use with numerical pseudoAxes.
 **/
void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					HklPseudoAxisEngineMode *mode)
{
	if(!self || !mode)
		return;

	self->mode = mode;
	hkl_pseudo_axis_engine_prepare_internal(self);
}

void hkl_pseudo_axis_engine_select_mode_by_name(HklPseudoAxisEngine *self,
						const char *name)
{
	HklPseudoAxisEngineMode *mode;

	if(!self || !name)
		return;

	list_for_each(&self->modes, mode, list)
		if(!strcmp(mode->info->name, name))
			hkl_pseudo_axis_engine_select_mode(self, mode);
}

/**
 * hkl_pseudo_axis_engine_initialize: (skip)
 * @self: the HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError to check for error's during the initialization
 *
 * initialize the HklPseudoAxisEngine
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_initialize(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->geometry || !self->detector || !self->sample
	   || !self->mode) {
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	/* a NULL initialize method is valid */
	if(self->mode->op->init
	   && !self->mode->op->init(self->mode,
				    self,
				    self->engines->geometry,
				    self->engines->detector,
				    self->engines->sample,
				    error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}

	hkl_assert(error == NULL || *error == NULL);

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_set: (skip)
 * @self: the HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError
 *
 * use the HklPseudoaxisEngine values to compute the real axes values.
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_set(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->geometry || !self->detector || !self->sample
	   || !self->mode || !self->mode->op->set){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	hkl_pseudo_axis_engine_prepare_internal(self);

	if (!self->mode->op->set(self->mode, self,
				 self->geometry,
				 self->detector,
				 self->sample,
				 error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	hkl_geometry_list_multiply(self->engines->geometries);
	hkl_geometry_list_multiply_from_range(self->engines->geometries);
	hkl_geometry_list_remove_invalid(self->engines->geometries);
	hkl_geometry_list_sort(self->engines->geometries, self->engines->geometry);

	if(self->engines->geometries->len == 0){
		hkl_error_set(error, "no remaining solutions");
		return HKL_FALSE;
	}

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_get: (skip)
 * @self: The HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError
 *
 * get the values of the pseudo-axes from the real-axes values
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_get(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->engines || !self->engines->geometry || !self->engines->detector
	   || !self->engines->sample || !self->mode || !self->mode->op->get){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	if (!self->mode->op->get(self->mode,
				 self,
				 self->engines->geometry,
				 self->engines->detector,
				 self->engines->sample,
				 error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_set_values: (skip)
 * @self: the PseudoAxisEngine
 * @values: the values to set
 *
 * set the values of th ePseudoAxes with the given values
 **/
void hkl_pseudo_axis_engine_set_values(HklPseudoAxisEngine *self,
				       double values[], uint len)
{
	HklPseudoAxis *pseudo_axis;
	uint i=0;

	hkl_assert(len == self->info->n_pseudo_axes);

	list_for_each(&self->pseudo_axes, pseudo_axis, list)
		pseudo_axis->parent.value = values[i++];
}

/**
 * hkl_pseudo_axis_engine_get_values:
 * @self: 
 * @: 
 * @len: 
 *
 * get the values of all the pseudo axes
 **/
void hkl_pseudo_axis_engine_get_values(HklPseudoAxisEngine *self,
				       double values[], uint *len)
{
	HklPseudoAxis *pseudo_axis;
	uint i = 0;

	assert(*len == self->info->n_pseudo_axes);

	list_for_each(&self->pseudo_axes, pseudo_axis, list)
		values[i++] = pseudo_axis->parent.value;
}

/**
 * hkl_pseudo_axis_engine_fprintf: (skip)
 * @f: the FILE
 * @self: the HklPseudoAxisEngine
 *
 * print to a FILE the HklPseudoAxisEngine
 **/
void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self)
{
	size_t i;
	HklPseudoAxis *pseudo_axis;

	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->info->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->info->name);

		for(i=0; i<self->mode->parameters_len; ++i){
			fprintf(f, "\n     ");
			hkl_parameter_fprintf(f, &self->mode->parameters[i]);
		}
		fprintf(f, "\n");
	}

	/* the pseudoAxes part */
	list_for_each(&self->pseudo_axes, pseudo_axis, list){
		fprintf(f, "\n     ");
		hkl_pseudo_axis_fprintf(f, pseudo_axis);
	}

	if(self->engines->geometries->len > 0){
		fprintf(f, "\n   ");
		hkl_geometry_list_fprintf(f, self->engines->geometries);
	}
	fprintf(f, "\n");
}

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

/**
 * hkl_pseudo_axis_engine_list_new: (skip)
 *
 * default constructor
 *
 * Returns:
 **/
HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new(void)
{
	HklPseudoAxisEngineList *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngineList);

	list_head_init(&self->engines);

	self->geometries = hkl_geometry_list_new();

	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;

	return self;
}

/**
 * hkl_pseudo_axis_engine_list_new_copy: (skip)
 * @self:
 *
 * dummy copy constructor for the binding
 *
 * Returns: (transfer none): NULL all the time the structure is non-copyable
 **/
const HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new_copy(const HklPseudoAxisEngineList *self)
{
	return NULL;
}

/**
 * hkl_pseudo_axis_engine_list_free: (skip)
 * @self: the #HklPseudoAxisEngineList to destroy
 *
 * destructor
 **/
void hkl_pseudo_axis_engine_list_free(HklPseudoAxisEngineList *self)
{
	hkl_pseudo_axis_engine_list_clear(self);
	hkl_geometry_list_free(self->geometries);
	free(self);
}

/**
 * hkl_pseudo_axis_engine_list_add: (skip)
 * @self: the engine list
 * @engine: the engine to add
 *
 * add an #HklPseudoAxisEngine to the #HklPseudoAxisEngineList
 *
 * Returns: HKL_SUCCESS or HKL_FAIL
 **/
int hkl_pseudo_axis_engine_list_add(HklPseudoAxisEngineList *self,
				    HklPseudoAxisEngine *engine)
{
	if (!engine)
		return HKL_FALSE;

	/* set the engines to access the Geometries list. */
	engine->engines = self;

	list_add_tail(&self->engines, &engine->list);

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_list_get_by_name: (skip)
 * @self: the engine list
 * @name: the name of the requested #HklPseudoAxisEngin
 *
 * get the #HklPseudoAxisEngine by its name from the list.
 *
 * Returns: (transfer none) (allow-none): the requested engine
 **/
HklPseudoAxisEngine *hkl_pseudo_axis_engine_list_get_by_name(HklPseudoAxisEngineList *self,
							     const char *name)
{
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list)
		if (!strcmp(engine->info->name, name))
			return engine;

	return NULL;
}

/**
 * hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name:
 * @self: the engine list
 * @name: the name of the requested #HklPseudoAxis
 *
 * Todo: test
 *
 * Returns: (transfer none) (allow-none): the requested #HklPseudoAxis
 **/
HklPseudoAxis *hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name(HklPseudoAxisEngineList *self,
								   const char *name)
{
	HklPseudoAxis *pseudo_axis;
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list)
		list_for_each(&engine->pseudo_axes, pseudo_axis, list)
			if (!strcmp(pseudo_axis->parent.name, name))
				return pseudo_axis;
	return NULL;
}

/**
 * hkl_pseudo_axis_engine_list_clear: (skip)
 * @self: the engine list to clear
 *
 * remove all engine from the engine list
 **/
void hkl_pseudo_axis_engine_list_clear(HklPseudoAxisEngineList *self)
{
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngine *next;

	list_for_each_safe(&self->engines, engine, next, list){
		list_del(&engine->list);
		hkl_pseudo_axis_engine_free(engine);
	}
}

/**
 * hkl_pseudo_axis_engine_list_init:
 * @self: the engine list
 * @geometry: the associated #HklGeometry
 * @detector: the associated #HklDetector
 * @sample: the associated #HklSample
 *
 * before using an engine list you must associate all engines to a
 * Geometry, a detector and a sample.
 **/
void hkl_pseudo_axis_engine_list_init(HklPseudoAxisEngineList *self,
				      HklGeometry *geometry,
				      HklDetector *detector,
				      HklSample *sample)
{
	HklPseudoAxisEngine *engine;

	self->geometry = geometry;
	self->detector = detector;
	self->sample = sample;

	list_for_each(&self->engines, engine, list)
		hkl_pseudo_axis_engine_prepare_internal(engine);
}

/**
 * hkl_pseudo_axis_engine_list_get:
 * @self: the list of #HklPseudoAxisEngine
 *
 * apply the get method to all the #HklPseudoAxisEngine of the list
 * after this it is possible to retrive all the #HklPseudoAxis values.
 *
 * Returns: HKL_SUCCESS or HKL_FAIL if one of the #HklPseudoAxisEngine
 * get method failed.
 **/
int hkl_pseudo_axis_engine_list_get(HklPseudoAxisEngineList *self)
{
	HklPseudoAxisEngine *engine;
	int res = HKL_TRUE;

	if (!self)
		return res;

	list_for_each(&self->engines, engine, list)
		res &= hkl_pseudo_axis_engine_get(engine, NULL);

	return res;
}

/**
 * hkl_pseudo_axis_engine_list_fprintf: (skip)
 * @f: the File
 * @self: the list
 *
 * print to a FILE the #HklPseudoAxisEngineList
 **/
void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
					 const HklPseudoAxisEngineList *self)
{
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list)
		hkl_pseudo_axis_engine_fprintf(f, engine);
}
