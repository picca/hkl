#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter.h>

HklParameter *hkl_parameter_new(char const *name,
				double min, double value, double max,
				int not_to_fit,
				HklUnit const *unit, HklUnit const *punit)
{
	HklParameter *parameter;

	parameter = malloc(sizeof(*parameter));
	if (!parameter)
		die("Cannot allocate memory for an HklParameter");

	if (hkl_parameter_set(parameter, name, min, value, max, not_to_fit, unit, punit)) {
		free(parameter);
		parameter = NULL;
	}

	return parameter;
}

HklParameter *hkl_parameter_new_copy(HklParameter const *self)
{
	HklParameter *parameter = NULL;

	parameter = malloc(sizeof(*parameter));
	if (!parameter)
		die("Cannot allocate memory for an HklParameter");

	*parameter = *self;

	return parameter;
}

int hkl_parameter_set(HklParameter *self, char const *name,
		      double min, double value, double max, int not_to_fit,
		      HklUnit const *unit, HklUnit const *punit)
{
	if (min <= value
	    && value <= max
	    && strcmp(name, "")
	    && hkl_unit_compatible(unit, punit)) {
		self->name = name;
		self->range.min = min;
		self->range.max = max;
		self->value = value;
		self->not_to_fit = not_to_fit;
		self->unit = unit;
		self->punit = punit;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

void hkl_parameter_free(HklParameter *self)
{
	free(self);
}

void hkl_parameter_randomize(HklParameter *self)
{
	if (!self->not_to_fit) {
		double alea = (double)rand() / (RAND_MAX + 1.);
		self->value = self->range.min
			+ (self->range.max - self->range.min) * alea;
	}
}

void hkl_parameter_fprintf(FILE *f, HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	fprintf(f, "\"%s\" : %f %s [%f : %f]",
		self->name,
		self->value * factor,
		self->punit->repr,
		self->range.min * factor,
		self->range.max * factor);
}
