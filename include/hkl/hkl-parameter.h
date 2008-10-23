#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <stdio.h>
#include <hkl/hkl-interval.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;

struct _HklParameter {
	const char *name;
	HklInterval range;
	double value;
	int not_to_fit;
};

extern HklParameter *hkl_parameter_new(char const *name,
		double min, double value, double max, int not_to_fit);
extern HklParameter *hkl_parameter_new_copy(HklParameter const *p);

extern int hkl_parameter_set(HklParameter *p, char const *name,
		double min, double value, double max, int not_to_fit);

extern void hkl_parameter_free(HklParameter *p);

extern void hkl_parameter_randomize(HklParameter *p);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
