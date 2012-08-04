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
	char const *name,
	const HklPseudoAxisEngineModeOperations *op,
	size_t n, ...)
{
	HklPseudoAxisEngineMode *self = NULL;
	va_list ap;
	size_t i;
	size_t n_p;
	size_t n_a;
	HklFunction *functions;
	HklParameter *parameters;
	const char **axes;

	/* extract the variable part of the method */

	va_start(ap, n);

	/* functions */
	functions = alloca(n * sizeof(*functions));
	for(i=0; i<n; ++i)
		functions[i] = va_arg(ap, HklFunction);

	/* parameters */
	n_p = va_arg(ap, size_t);
	parameters = alloca(n_p * sizeof(*parameters));
	for(i=0; i<n_p; ++i)
		parameters[i] = va_arg(ap, HklParameter);

	/* axes */
	n_a = va_arg(ap, size_t);
	axes = alloca(n_a * sizeof(*axes));
	for(i=0; i<n_a; ++i)
		axes[i] = va_arg(ap, char const *);
	va_end(ap);

	self = HKL_MALLOC(HklPseudoAxisEngineMode);

	self->functions = NULL;
	self->parameters = NULL;
	self->axes = NULL;
	hkl_pseudo_axis_engine_mode_init(self, name,
					 op,
					 n, functions,
					 n_p, parameters,
					 n_a, axes);


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
	char const *name,
	const HklPseudoAxisEngineModeOperations *op,
	size_t functions_len, HklFunction functions[],
	size_t parameters_len, HklParameter parameters[],
	size_t axes_len, char const *axes[])
{
	size_t i;

	/* ensure part */
	if (!self)
		return HKL_FALSE;

	self->name = name;
	self->op = op;

	/* functions */
	self->functions_len = functions_len;
	self->functions = realloc(self->functions, sizeof(*self->functions) * self->functions_len);
	memcpy(self->functions, functions, sizeof(*self->functions) * self->functions_len);

	/* parameters */
	self->parameters = realloc(self->parameters, sizeof(*self->parameters) * parameters_len);
	self->parameters_len = parameters_len;
	memcpy(self->parameters, parameters, sizeof(*self->parameters) * parameters_len);

	/* axes */
	self->axes = realloc(self->axes, sizeof(*self->axes) * axes_len);
	self->axes_len = axes_len;
	memcpy(self->axes, axes, sizeof(*self->axes) * axes_len);

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
	free(self->functions);
	self->functions_len = 0;
	free(self->parameters);
	self->parameters_len = 0;
	free(self->axes);
	self->axes_len = 0;

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

	fprintf(f, "mode: \"%s\"\n", self->name);
	fprintf(f, "initialize: %p\n", self->op->init);
	fprintf(f, "get: %p\n", self->op->get);
	fprintf(f, "set: %p\n", self->op->set);
	if(self->functions){
		fprintf(f, "functions:");
		for(i=0; i<self->functions_len; ++i)
			fprintf(f, " %p", self->functions[i]);
		fprintf(f, "\n");
	}
	if(self->parameters)
		for(i=0; i<self->parameters_len; ++i){
			hkl_parameter_fprintf(f, &self->parameters[i]);
			fprintf(f, "\n");
		}
	if(self->axes){
		fprintf(f, "axes names:");
		for(i=0; i<self->axes_len; ++i)
			fprintf(f, " %s", self->axes[i]);
		fprintf(f, "\n");
	}
	hkl_geometry_fprintf(f, self->geometry_init);
	hkl_detector_fprintf(f, self->detector_init);
	hkl_sample_fprintf(f, self->sample_init);
}

/***********************/
/* HklPseudoAxisEngine */
/***********************/

/**
 * hkl_pseudo_axis_engine_new: (skip)
 * @name: the name of the engine
 * @n: the number of PseudoAxis of this PseudoAxisEngine
 * @...: the names of the pseudo-axes as string. 
 *
 * Create a new Engine
 * self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");
 *
 * Returns: 
 **/
HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(char const *name,
						size_t n, ...)
{
	va_list ap;
	size_t i;

	HklPseudoAxisEngine *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngine);

	self->name = name;

	/* create the pseudoAxes */
	self->pseudoAxes = malloc(sizeof(*self->pseudoAxes) * n);
	self->pseudoAxes_len = n;
	va_start(ap, n);
	for(i=0; i<n; ++i){
		HklParameter parameter;

		hkl_parameter_init(&parameter, va_arg(ap, const char*),
				   -RAND_MAX, 0., RAND_MAX,
				   HKL_FALSE, HKL_TRUE,
				   NULL, NULL);
		self->pseudoAxes[i] = hkl_pseudo_axis_new(&parameter, self);
	}
	va_end(ap);

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

	if (self->geometry)
		hkl_geometry_free(self->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);

	if(self->sample)
		hkl_sample_free(self->sample);

	/* release the axes memory but the HklAxis are not owned by the HklPseudoAxisEngine*/
	free(self->axes);
	self->axes_len = 0;

	/* release the mode added */
	for(i=0; i<self->modes_len; ++i)
		hkl_pseudo_axis_engine_mode_free(self->modes[i]);
	free(self->modes);
	self->mode = NULL;
	self->modes_len = 0;

	/* release the HklPseudoAxe memory */
	for(i=0; i<self->pseudoAxes_len; ++i)
		hkl_pseudo_axis_free(self->pseudoAxes[i]);
	free(self->pseudoAxes);
	self->pseudoAxes = NULL;
	self->pseudoAxes_len = 0;

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
	self->modes = realloc(self->modes, sizeof(*self->modes) * (self->modes_len + 1));
	self->modes[self->modes_len++] = mode;
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
	size_t i;

	/* copy the axes configuration into the engine->geometry */
	for(i=0; i<self->axes_len; ++i)
		hkl_axis_set_value(self->axes[i], gsl_sf_angle_restrict_symm(x[i]));

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}

static void hkl_pseudo_axis_engine_prepare_internal(HklPseudoAxisEngine *self)
{
	size_t i;
	size_t len;

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
		len = self->mode->axes_len;
		self->axes = realloc(self->axes, sizeof(*self->axes) * len);
		self->axes_len = len;
		for(i=0; i<len; ++i)
			self->axes[i] = hkl_geometry_get_axis_by_name(self->geometry,
								      self->mode->axes[i]);
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
					size_t idx)
{
	if(!self || idx > self->modes_len)
		return;

	self->mode = self->modes[idx];
	hkl_pseudo_axis_engine_prepare_internal(self);
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
 * hkl_pseudo_axis_engine_fprintf: (skip)
 * @f: the FILE
 * @self: the HklPseudoAxisEngine
 *
 * print to a FILE the HklPseudoAxisEngine
 **/
void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self)
{
	size_t i;

	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->name);

		for(i=0; i<self->mode->parameters_len; ++i){
			fprintf(f, "\n     ");
			hkl_parameter_fprintf(f, &self->mode->parameters[i]);
		}
		fprintf(f, "\n");
	}

	/* the pseudoAxes part */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		fprintf(f, "\n     ");
		hkl_pseudo_axis_fprintf(f, self->pseudoAxes[i]);
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
		if (!strcmp(engine->name, name))
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
	size_t j;
	HklPseudoAxis *pseudo = NULL;
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list){
		for(j=0; j<engine->pseudoAxes_len; ++j){
			HklParameter *parameter;

			parameter = (HklParameter *)engine->pseudoAxes[j];
			if (!strcmp(parameter->name, name))
				return engine->pseudoAxes[j];
		}
	}
	return pseudo;
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
