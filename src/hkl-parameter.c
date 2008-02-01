#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter.h>

HklParameter *hkl_parameter_new(char const *name,
		double min, double value, double max, int not_to_fit)
{
	HklParameter *p;

	p = malloc(sizeof(HklParameter));
	if (!p)
		die("Cannot allocate memory for an HklParameter");

	if (hkl_parameter_set(p, name, min, value, max, not_to_fit))
		return p;
	else {
		free(p);
		return NULL;
	}
}

HklParameter *hkl_parameter_new_copy(HklParameter const *p)
{
	HklParameter *copy = NULL;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Cannot allocate memory for an HklParameter");

	*copy = *p;

	return copy;
}

int hkl_parameter_set(HklParameter *p, char const *name,
		double min, double value, double max, int not_to_fit)
{
	if (min <= value && value <= max && strcmp(name, "")) {
		p->name = name;
		p->range.min = min;
		p->range.max = max;
		p->value = value;
		p->not_to_fit = not_to_fit;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

void hkl_parameter_free(HklParameter *p)
{
	free(p);
}

void hkl_parameter_randomize(HklParameter *p)
{
	if (!p->not_to_fit) {
		double alea = (double)rand() / (RAND_MAX + 1.);
		p->value = p->range.min + (p->range.max - p->range.min) * alea;
	}
}
