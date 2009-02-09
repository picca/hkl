#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <stdio.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-unit.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;

struct _HklParameter {
	const char *name;
	HklInterval range;
	double value;
	HklUnit const *unit;
	HklUnit const *punit;
	int not_to_fit;
	int changed;
};

extern HklParameter *hkl_parameter_new(char const *name,
				       double min, double value, double max,
				       int not_to_fit, int changed,
				       HklUnit const *unit,
				       HklUnit const *punit);

extern HklParameter *hkl_parameter_new_copy(HklParameter const *self);

extern int hkl_parameter_init(HklParameter *self, char const *name,
			      double min, double value, double max,
			      int not_to_fit, int changed,
			      HklUnit const *unit, HklUnit const *punit);

extern void hkl_parameter_free(HklParameter *self);

extern double hkl_parameter_get_value(HklParameter const *self);

extern int hkl_parameter_set_value(HklParameter *self, double value);

extern void hkl_parameter_randomize(HklParameter *self);

extern void hkl_parameter_fprintf(FILE *f, HklParameter *self);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
