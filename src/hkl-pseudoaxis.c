#include <string.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

/*****************************/
/* HklPseudoAxisEngineGetSet */
/*****************************/

HklPseudoAxisEngineGetSet *hkl_pseudo_axis_engine_get_set_new(
		char const *name,
		HklPseudoAxisEngineGetterFunc get,
		HklPseudoAxisEngineSetterFunc set,
		size_t n, ...)
{
	HklPseudoAxisEngineGetSet *self = NULL;
	va_list ap;
	size_t i;
	size_t len;

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineGetSet");

	self->name = name;
	self->get = get;
	self->set = set;

	va_start(ap, n);
	/* parameters */
	if (n) {
		self->parameters = calloc(n, sizeof(HklParameter));
		self->parameters_len = n;
		for(i=0; i<n; ++i)
			self->parameters[i] = *va_arg(ap, HklParameter*);
	}

	/* axes */
	len = va_arg(ap, size_t);
	self->axes_names = calloc(len, sizeof(char const *));
	self->axes_names_len = len;
	for(i=0; i<len; ++i)
		self->axes_names[i] = va_arg(ap, char const *);
	va_end(ap);

	return self;
}


void hkl_pseudo_axis_engine_get_set_free(HklPseudoAxisEngineGetSet *self)
{
	if(self->parameters_len) {
		self->parameters_len = 0;
		free(self->parameters);
		self->parameters = NULL;
	}

	if(self->axes_names_len) {
		self->axes_names_len = 0;
		free(self->axes_names);
		self->axes_names = NULL;
	}
	free(self);
}

/*******************/
/* pseudoAxeEngine */
/*******************/

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name,
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
	self->pseudoAxes = malloc(n * sizeof(HklPseudoAxis));
	self->pseudoAxes_len = n;
	va_start(ap, n);
	for(i=0; i<n; ++i) {
		self->pseudoAxes[i].name = va_arg(ap, const char*);
		self->pseudoAxes[i].engine = self;
	}
	va_end(ap);

	return self;
}

void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *self)
{
	size_t i;

	if (self->geometry)
		hkl_geometry_free(self->geometry);
	/* release the axes memory */
	if (self->axes_len) {
		free(self->axes);
		self->axes = NULL;
		self->axes_len = 0;
	}
	/* release the getset added */
	if (self->getsets_len) {
		for(i=0; i<self->getsets_len; ++i)
			hkl_pseudo_axis_engine_get_set_free(self->getsets[i]);
		self->getsets_len = 0;
		free(self->getsets);
		self->getsets = NULL;
	}
	/* release the HklPseudoAxe memory */
	if (self->pseudoAxes_len) {
		free(self->pseudoAxes);
		self->pseudoAxes = NULL;
		self->pseudoAxes_len = 0;
	}
	/* release the geometries allocated during calculations */
	if (self->geometries_alloc) {
		for(i=0; i<self->geometries_alloc; ++i)
			hkl_geometry_free(self->geometries[i]);
		free(self->geometries);
		self->geometries = NULL;
		self->geometries_alloc = self->geometries_len = 0;
	}
	free(self);
}

void hkl_pseudoAxisEngine_add_get_set(HklPseudoAxisEngine *self,
		HklPseudoAxisEngineGetSet *getset)
{
	size_t n = self->getsets_len++;
	self->getsets = realloc(self->getsets, 
			self->getsets_len*sizeof(HklPseudoAxisEngineGetSet*));
	self->getsets[n] = getset;
}

/** 
 * @brief this method Add a geometry to the geometries
 * 
 * @param self The current PseudoAxeEngine 
 * @param x A vector of double with the axes values to put in the geometry.
 *
 * This method try to be clever by allocating memory only if the current
 * length of the geometries is not large enought. Then it just set the
 * geometry axes and copy it to the right geometries.
 */
void hkl_pseudoAxisEngine_add_geometry(HklPseudoAxisEngine *self, double const x[])
{
	size_t i;
	HklGeometry *geometry;

	// first check if we can get an old geometry.
	if (self->geometries_len == self->geometries_alloc) {
		self->geometries_alloc = alloc_nr(self->geometries_alloc);
		self->geometries = realloc(self->geometries,
				self->geometries_alloc * sizeof(HklGeometry*));
		for(i=self->geometries_len; i<self->geometries_alloc; i++)
			self->geometries[i] = hkl_geometry_new_copy(self->geometry);
	}

	/* copy the axes configuration into the engine->geometry*/
	for(i=0; i<self->axes_len; ++i)
		self->axes[i]->config.value = x[i];

	/* put the axes configuration from engine->geometry -> geometry */
	geometry = self->geometries[self->geometries_len++];
	hkl_geometry_init_geometry(geometry, self->geometry);
}

void hkl_pseudoAxisEngine_select_get_set(HklPseudoAxisEngine *self,
		size_t idx)
{
	self->getset = self->getsets[idx];
}

/** 
 * @brief update the geometry, detector and sample internals.
 * 
 * @param self The current PseudoAxeEngine
 * @param geometry the geometry to initialize the self->geometry
 * @param detector idem for the geometry
 * @param sample idem for the sample
 *
 * This method also populate the self->axes from the getset->axes_names.
 * this is to speed the computation of the numerical axes. this method is
 * usually only use with numerical pseudoAxes.
 */
void hkl_pseudoAxeEngine_prepare_internal(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	size_t i;

	/* set */
	if(self->geometry)
		hkl_geometry_init_geometry(self->geometry, geometry);
	else
		self->geometry = hkl_geometry_new_copy(geometry);
	self->detector = detector;
	self->sample = sample;

	// fill the axes member from the function
	if (self->axes_len)
		free(self->axes), self->axes = NULL, self->axes_len = 0;
	self->axes_len = self->getset->axes_names_len;
	self->axes = malloc(self->axes_len * sizeof(HklAxis *));
	for(i=0; i<self->axes_len; ++i)
		self->axes[i] = hkl_geometry_get_axis_by_name(self->geometry,
				self->getset->axes_names[i]);

	// reset the geometries len
	self->geometries_len = 0;
}

int hkl_pseudoAxisEngine_setter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample)
{
	return self->getset->set(self, geom, det, sample);
}

void hkl_pseudoAxisEngine_getter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample)
{
	self->getset->get(self, geom, det, sample);
}

void hkl_pseudoAxisEngine_fprintf(HklPseudoAxisEngine *self, FILE *f)
{
	size_t i, j;
	double value;

	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->name);

	/* getset */
	if (self->getset) {
		fprintf(f, " %s", self->getset->name);

		for(i=0; i<self->getset->parameters_len; ++i)
			fprintf(f, " \"%s\" = %g",
					self->getset->parameters[i].name,
					self->getset->parameters[i].value);
	}

	/* the pseudoAxes part */
	fprintf(f, "\n   ");
	for(i=0; i<self->pseudoAxes_len; ++i)
		fprintf(f, " \"%s\" : %f", self->pseudoAxes[i].name, self->pseudoAxes[i].config.value);

	/* axes names */
	if (self->geometry) {
		fprintf(f, "\n   ");
		for(i=0; i<self->geometry->axes_len; ++i)
			fprintf(f, "%9s", self->geometry->axes[i]->name);

		/* geometries */
		fprintf(f, "\n");
		for(i=0; i<self->geometries_len; ++i) {
			fprintf(f, "%d :", i);
			for(j=0; j<self->geometry->axes_len; ++j) {
				value = gsl_sf_angle_restrict_symm(self->geometries[i]->axes[j]->config.value);
				fprintf(f, " % 9.6g", value * HKL_RADTODEG);
			}
			fprintf(f, "\n");
		}
	} else
		fprintf(stdout, "\n");
}
