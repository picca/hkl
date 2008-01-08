#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter.h>

HklParameter* hkl_parameter_new(char const *name, double min, double value, double max, int not_to_fit)
{
	HklParameter *parameter;

	parameter = malloc(sizeof(HklParameter));
	if (!parameter)
		die("Cannot allocate memory for an HklParameter");
	if (hkl_parameter_init(parameter, name, min, value, max, not_to_fit))
		return parameter;
	else {
		free(parameter);
		return NULL;
	}
}

int hkl_parameter_init(HklParameter *parameter, char const *name, double min, double value, double max, int not_to_fit)
{
	if (min <= value && value <= max && strcmp(name, "")) {
		parameter->name = name;
		parameter->range.min = min;
		parameter->range.max = max;
		parameter->value = value;
		parameter->not_to_fit = not_to_fit;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

void hkl_parameter_release(HklParameter *parameter)
{
}

void hkl_parameter_free(HklParameter *parameter)
{
	free(parameter);
}

void hkl_parameter_randomize(HklParameter *parameter)
{
	if (!parameter->not_to_fit)
		parameter->value = parameter->range.min + (parameter->range.max - parameter->range.min) * (double)rand() / (RAND_MAX + 1.);
}
