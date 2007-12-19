#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter.h>

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

void hkl_parameter_randomize(HklParameter *parameter)
{
	if (!parameter->not_to_fit)
		parameter->value = parameter->range.min + (parameter->range.max - parameter->range.min) * (double)rand() / (RAND_MAX + 1.);
}
