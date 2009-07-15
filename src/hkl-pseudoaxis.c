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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

/*****************/
/* HklPseudoAxis */
/*****************/

HklPseudoAxis *hkl_pseudo_axis_new(HklParameter const *parameter,
				   HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *self;

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineMode");

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
	HklPseudoAxisEngineInitFunc init,
	HklPseudoAxisEngineGetterFunc get,
	HklPseudoAxisEngineSetterFunc set,
	size_t n, ...)
{
	HklPseudoAxisEngineMode *self = NULL;
	va_list ap;
	size_t i;
	size_t len;

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineMode");

	self->name = name;
	self->init = init;
	self->get = get;
	self->set = set;

	va_start(ap, n);
	/* parameters */
	if (n) {
		HKL_LIST_ALLOC(self->parameters, n);
		for(i=0; i<n; ++i)
			self->parameters[i] = *va_arg(ap, HklParameter*);
	}

	/* axes */
	len = va_arg(ap, size_t);
	HKL_LIST_ALLOC(self->axes_names, len);
	for(i=0; i<len; ++i)
		self->axes_names[i] = va_arg(ap, char const *);
	va_end(ap);

	/* init part */
	self->geometry_init = NULL;
	self->sample_init = NULL;

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
	HklPseudoAxisEngineInitFunc init,
	HklPseudoAxisEngineGetterFunc get,
	HklPseudoAxisEngineSetterFunc set,
	size_t parameters_names_len, char const *parameters_names[],
	size_t axes_names_len, char const *axes_names[])
{
	// ensure part
	if (!self)
		return HKL_FAIL;

	size_t i;

	self->name = name;
	self->init = init;
	self->get = get;
	self->set = set;

	// parameters
	HKL_LIST_RESIZE(self->parameters, parameters_names_len);
	for(i=0; i<parameters_names_len; ++i)
		self->parameters[i].name = parameters_names[i];

	/* axes */
	HKL_LIST_RESIZE(self->axes_names, axes_names_len);
	for(i=0; i<axes_names_len; ++i)
		self->axes_names[i] = axes_names[i];

	/* init part */
	self->geometry_init = NULL;
	self->sample_init = NULL;

	return HKL_SUCCESS;
}

/**
 * @brief release the memory of an HklPseudoAxisEngineMode
 */
void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self)
{
	HKL_LIST_FREE(self->parameters);
	HKL_LIST_FREE(self->axes_names);

	if(self->geometry_init){
		hkl_geometry_free(self->geometry_init);
		self->geometry_init = NULL;
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

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngine");

	self->name = name;

	// create the pseudoAxes
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

void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					 size_t idx)
{
	self->mode = self->modes[idx];
}

/**
 * @brief update the geometry, detector and sample internals.
 *
 * @param self The current PseudoAxeEngine
 * @param geometry the geometry to initialize the self->geometry
 * @param detector idem for the geometry
 * @param sample idem for the sample
 *
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes. this method is
 * usually only use with numerical pseudoAxes.
 */
void hkl_pseudo_axis_engine_prepare_internal(HklPseudoAxisEngine *self,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample)
{
	size_t i;
	size_t len;

	/* set */
	if(self->geometry)
		hkl_geometry_init_geometry(self->geometry, geometry);
	else
		self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = detector;
	self->sample = sample;

	// fill the axes member from the function
	len = HKL_LIST_LEN(self->mode->axes_names);
	HKL_LIST_RESIZE(self->axes, len);
	for(i=0; i<len; ++i)
		self->axes[i] = hkl_geometry_get_axis_by_name(self->geometry,
							      self->mode->axes_names[i]);

	// reset the geometries len
	hkl_geometry_list_reset(self->engines->geometries);
}

int hkl_pseudo_axis_engine_init(HklPseudoAxisEngine *self, HklGeometry *geometry,
				HklDetector *detector, HklSample *sample)
{
	int res = HKL_FAIL;

	if(!self || !geometry || !detector || !sample)
		return res;

	if (self->mode && self->mode->init)
		res = self->mode->init(self, geometry, detector, sample);

	return res;
}

int hkl_pseudo_axis_engine_setter(HklPseudoAxisEngine *self, HklGeometry *geometry,
				  HklDetector *detector, HklSample *sample)
{
	int res = HKL_FAIL;

	if(!self || !geometry || !detector || !sample)
		return res;

	if (self->mode && self->mode->set)
		res = self->mode->set(self, geometry, detector, sample);

	hkl_geometry_list_multiply(self->engines->geometries);
	hkl_geometry_list_multiply_from_range(self->engines->geometries);
	hkl_geometry_list_sort(self->engines->geometries, geometry);

	return res;
}

int hkl_pseudo_axis_engine_getter(HklPseudoAxisEngine *self, HklGeometry *geometry,
				  HklDetector *detector, HklSample *sample)
{
	int res = HKL_FAIL;

	if(!self || !geometry || !detector || !sample)
		return res;

	if (self->mode && self->mode->get)
		res = self->mode->get(self, geometry, detector, sample);

	return res;
}

void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self)
{
	size_t i;

	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->name);

		for(i=0; i<HKL_LIST_LEN(self->mode->parameters); ++i)
			fprintf(f, " \"%s\" = %g",
				self->mode->parameters[i].name,
				self->mode->parameters[i].value);
	}

	/* the pseudoAxes part */
	for(i=0; i<HKL_LIST_LEN(self->pseudoAxes); ++i) {
		fprintf(f, "\n     ");
		hkl_pseudo_axis_fprintf(f, self->pseudoAxes[i]);
	}

	if(HKL_LIST_LEN(self->engines->geometries->geometries)){
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

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineList");
	HKL_LIST_INIT(self->engines);

	self->geometries = hkl_geometry_list_new();

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

	engine->engines = self; // set the engines to access the Geometries list.

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

int hkl_pseudo_axis_engine_list_getter(HklPseudoAxisEngineList *self,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample)
{
	size_t i;
	int res = HKL_SUCCESS;

	if (!geometry || !detector || !sample)
		return res;

	for(i=0; i<HKL_LIST_LEN(self->engines); ++i){
		if (!hkl_pseudo_axis_engine_getter(self->engines[i],
						   geometry,
						   detector,
						   sample))
			res = HKL_FAIL;
	}

	return res;
}

void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
					 HklPseudoAxisEngineList const *self)
{
	size_t i;
	for(i=0; i<HKL_LIST_LEN(self->engines); ++i)
		hkl_pseudo_axis_engine_fprintf(f, self->engines[i]);
}
