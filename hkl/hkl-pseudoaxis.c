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
#include <alloca.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

/*****************/
/* HklPseudoAxis */
/*****************/

HklPseudoAxis *hkl_pseudo_axis_new(HklParameter const *parameter,
				   HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *self;

	self = HKL_MALLOC(HklPseudoAxis);

	hkl_pseudo_axis_init(self, parameter, engine);

	return self;
}

void hkl_pseudo_axis_init(HklPseudoAxis *self,
			  HklParameter const *parameter,
			  HklPseudoAxisEngine *engine)
{
	self->parent = *parameter;
	self->engine = engine;
}

void hkl_pseudo_axis_free(HklPseudoAxis *self)
{
	if(self)
		free(self);
}

void hkl_pseudo_axis_fprintf(FILE *f, HklPseudoAxis *self)
{
	hkl_parameter_fprintf(f, &self->parent);
	fprintf(f, " %p", self->engine);
}

/*****************************/
/* HklPseudoAxisEngineMode */
/*****************************/

/**
 * @brief this method create an HklPseudoAxisEngineMode
 * @param name The name of this HklPseudoAxisEngineMode.
 * @param get The get method.
 * @param set the set method.
 * @param n the number of parameters.
 * @param ... rest of the parameters.
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
 *	hkl_pseudo_axis_engine_getter_func_hkl,
 *	hkl_pseudo_axis_engine_setter_func_constant_omega,
 *	1, &parameter,
 *	4, "komega", "kappa", "kphi", "tth");
 */
HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_new(
	char const *name,
	HklPseudoAxisEngineModeFunc initialize,
	HklPseudoAxisEngineModeFunc get,
	HklPseudoAxisEngineModeFunc set,
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

	hkl_pseudo_axis_engine_mode_init(self, name, initialize, get, set,
					 n, functions,
					 n_p, parameters,
					 n_a, axes);


	return self;
}

/**
 * @brief this method initialize an HklPseudoAxisEngineMode
 * @param name The name of this HklPseudoAxisEngineMode.
 * @param init the init method.
 * @param get The get method.
 * @param set the set method.
 * @param parameters_names_len the number of parameters.
 * @param parameters_name an array with the parameters names.
 * @param axes_names_len the length of the axes names.
 * @param axes_names an array with tha axes names.
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
 *	hkl_pseudo_axis_engine_getter_func_hkl,
 *	hkl_pseudo_axis_engine_setter_func_constant_omega,
 *	1, &parameter,
 *	4, "komega", "kappa", "kphi", "tth");
 */
int hkl_pseudo_axis_engine_mode_init(
	HklPseudoAxisEngineMode *self,
	char const *name,
	HklPseudoAxisEngineModeFunc initialize,
	HklPseudoAxisEngineModeFunc get,
	HklPseudoAxisEngineModeFunc set,
	size_t functions_len, HklFunction functions[],
	size_t parameters_len, HklParameter parameters[],
	size_t axes_names_len, char const *axes_names[])
{
	size_t i;

	/* ensure part */
	if (!self)
		return HKL_FAIL;

	self->name = name;
	self->initialize = initialize;
	self->get = get;
	self->set = set;

	/* functions */
	HKL_LIST_RESIZE(self->functions, functions_len);
	for(i=0; i<functions_len; ++i)
		self->functions[i] = functions[i];

	/* parameters */
	HKL_LIST_RESIZE(self->parameters, parameters_len);
	for(i=0; i<parameters_len; ++i)
		self->parameters[i] = parameters[i];

	/* axes */
	HKL_LIST_RESIZE(self->axes_names, axes_names_len);
	for(i=0; i<axes_names_len; ++i)
		self->axes_names[i] = axes_names[i];

	/* init part */
	self->geometry_init = NULL;
	self->detector_init = NULL;
	self->sample_init = NULL;

	return HKL_SUCCESS;
}

/**
 * @brief release the memory of an HklPseudoAxisEngineMode
 */
void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self)
{
	HKL_LIST_FREE(self->functions);
	HKL_LIST_FREE(self->parameters);
	HKL_LIST_FREE(self->axes_names);

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

/***********************/
/* HklPseudoAxisEngine */
/***********************/

/**
 * @brief create a new HklPseudoAxisEngine
 * @param name The name of this engine
 * @param n the number of HklPseudoAxis of the engine
 * @param ... the names of thoses pseudo-axes.
 *
 * self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");
 */
HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(char const *name,
						size_t n, ...)
{
	va_list ap;
	size_t i;

	HklPseudoAxisEngine *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngine);

	self->name = name;

	/* create the pseudoAxes */
	HKL_LIST_ALLOC(self->pseudoAxes, n);
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
 * @brief Release the memory of an HklPseudoAxisEngine
 */
void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self)
{
	if (self->geometry)
		hkl_geometry_free(self->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);

	if(self->sample)
		hkl_sample_free(self->sample);

	/* release the axes memory */
	HKL_LIST_FREE(self->axes);

	/* release the mode added */
	HKL_LIST_FREE_DESTRUCTOR(self->modes, hkl_pseudo_axis_engine_mode_free);

	/* release the HklPseudoAxe memory */
	HKL_LIST_FREE_DESTRUCTOR(self->pseudoAxes, hkl_pseudo_axis_free);

	free(self);
}

/**
 * @brief add an HklPseudoAxisEngineMode to an engine.
 * @param self the engine
 * @param mode the getter and setter to add.
 */
void hkl_pseudo_axis_engine_add_mode(HklPseudoAxisEngine *self,
					HklPseudoAxisEngineMode *mode)
{
	HKL_LIST_ADD_VALUE(self->modes, mode);
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
void hkl_pseudo_axis_engine_add_geometry(HklPseudoAxisEngine *self,
					 double const x[])
{
	size_t i;

	/* copy the axes configuration into the engine->geometry */
	for(i=0; i<HKL_LIST_LEN(self->axes); ++i)
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
		len = HKL_LIST_LEN(self->mode->axes_names);
		HKL_LIST_RESIZE(self->axes, len);
		for(i=0; i<len; ++i)
			self->axes[i] = hkl_geometry_get_axis_by_name(self->geometry,
								      self->mode->axes_names[i]);
	}

	/* reset the geometries len */
	hkl_geometry_list_reset(self->engines->geometries);
}

/*
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes. this method is
 * usually only use with numerical pseudoAxes.
 */
void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					size_t idx)
{
	if(!self || idx > HKL_LIST_LEN(self->modes))
		return;

	self->mode = self->modes[idx];
	hkl_pseudo_axis_engine_prepare_internal(self);
}

int hkl_pseudo_axis_engine_initialize(HklPseudoAxisEngine *self, HklError **error)
{
	int res = HKL_FAIL;

	if(!self || !self->geometry || !self->detector || !self->sample)
		return res;

	if (self->mode && self->mode->initialize){
		HklError *tmp_error;

		tmp_error = NULL;
		res = self->mode->initialize(self->mode,
					     self,
					     self->engines->geometry,
					     self->engines->detector,
					     self->engines->sample,
					     &tmp_error);
		if(tmp_error != NULL)
			hkl_error_propagate(error, tmp_error);
	}

	return res;
}

int hkl_pseudo_axis_engine_set(HklPseudoAxisEngine *self, HklError **error)
{
	int res = HKL_FAIL;

	if(!self || !self->geometry || !self->detector || !self->sample)
		return res;

	hkl_pseudo_axis_engine_prepare_internal(self);

	if (self->mode && self->mode->set){
		HklError *tmp_error;

		tmp_error = NULL;
		res = self->mode->set(self->mode, self,
				      self->geometry,
				      self->detector,
				      self->sample,
				      &tmp_error);
		if(tmp_error != NULL)
			hkl_error_propagate(error, tmp_error);
	}

	hkl_geometry_list_multiply(self->engines->geometries);
	hkl_geometry_list_multiply_from_range(self->engines->geometries);
	hkl_geometry_list_remove_invalid(self->engines->geometries);
	hkl_geometry_list_sort(self->engines->geometries, self->engines->geometry);

	if(hkl_geometry_list_is_empty(self->engines->geometries))
		res = HKL_FAIL;

	return res;
}

int hkl_pseudo_axis_engine_get(HklPseudoAxisEngine *self, HklError **error)
{
	int res = HKL_FAIL;

	if(!self 
	   || !self->engines
	   || !self->engines->geometry
	   || !self->engines->detector
	   || !self->engines->sample)
		return res;

	if (self->mode && self->mode->get){
		HklError *tmp_error;

		tmp_error = NULL;
		res = self->mode->get(self->mode, 
				      self,
				      self->engines->geometry,
				      self->engines->detector,
				      self->engines->sample,
				      &tmp_error);
		if(tmp_error != NULL)
			hkl_error_propagate(error, tmp_error);
	}

	return res;
}

void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self)
{
	size_t i;

	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->name);

		for(i=0; i<HKL_LIST_LEN(self->mode->parameters); ++i){
			fprintf(f, "\n     ");
			hkl_parameter_fprintf(f, &self->mode->parameters[i]);
		}
		fprintf(f, "\n");
	}

	/* the pseudoAxes part */
	for(i=0; i<HKL_LIST_LEN(self->pseudoAxes); ++i) {
		fprintf(f, "\n     ");
		hkl_pseudo_axis_fprintf(f, self->pseudoAxes[i]);
	}

	if(!hkl_geometry_list_is_empty(self->engines->geometries)){
		fprintf(f, "\n   ");
		hkl_geometry_list_fprintf(f, self->engines->geometries);
	}
	fprintf(f, "\n");
}

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new(void)
{
	HklPseudoAxisEngineList *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngineList);

	HKL_LIST_INIT(self->engines);

	self->geometries = hkl_geometry_list_new();

	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;

	return self;
}

void hkl_pseudo_axis_engine_list_free(HklPseudoAxisEngineList *self)
{
	hkl_pseudo_axis_engine_list_clear(self);
	hkl_geometry_list_free(self->geometries);
	free(self);
}

int hkl_pseudo_axis_engine_list_add(HklPseudoAxisEngineList *self,
				    HklPseudoAxisEngine *engine)
{
	if (!engine)
		return HKL_FAIL;

	/* set the engines to access the Geometries list. */
	engine->engines = self;

	HKL_LIST_ADD_VALUE(self->engines, engine);

	return HKL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_list_get_by_name(HklPseudoAxisEngineList *self,
							     char const *name)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(self->engines); ++i)
		if (!strcmp(self->engines[i]->name, name))
			return self->engines[i];

	return NULL;
}

/* TODO test */
HklPseudoAxis *hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name(HklPseudoAxisEngineList *self,
								   char const *name)
{
	size_t i, j;
	HklPseudoAxis *pseudo = NULL;

	for(i=0; i<HKL_LIST_LEN(self->engines); ++i){
		HklPseudoAxisEngine *engine;

		engine = self->engines[i];
		for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
			HklParameter *parameter;

			parameter = (HklParameter *)engine->pseudoAxes[j];
			if (!strcmp(parameter->name, name))
				return engine->pseudoAxes[j];
		}
	}
	return pseudo;
}

void hkl_pseudo_axis_engine_list_clear(HklPseudoAxisEngineList *self)
{
	HKL_LIST_FREE_DESTRUCTOR(self->engines, hkl_pseudo_axis_engine_free);
}

void hkl_pseudo_axis_engine_list_init(HklPseudoAxisEngineList *self,
				      HklGeometry *geometry,
				      HklDetector *detector,
				      HklSample *sample)
{
	size_t i;

	self->geometry = geometry;
	self->detector = detector;
	self->sample = sample;

	for(i=0; i<HKL_LIST_LEN(self->engines); ++i)
		hkl_pseudo_axis_engine_prepare_internal(self->engines[i]);
}

int hkl_pseudo_axis_engine_list_get(HklPseudoAxisEngineList *self)
{
	size_t i;
	int res = HKL_SUCCESS;

	if (!self)
		return res;

	for(i=0; i<HKL_LIST_LEN(self->engines); ++i)
		if (!hkl_pseudo_axis_engine_get(self->engines[i], NULL))
			res = HKL_FAIL;

	return res;
}

void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
					 HklPseudoAxisEngineList const *self)
{
	size_t i;
	for(i=0; i<HKL_LIST_LEN(self->engines); ++i)
		hkl_pseudo_axis_engine_fprintf(f, self->engines[i]);
}
