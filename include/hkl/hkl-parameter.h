#ifndef __HKL_PARAMETER_H__
#define __HKL_PARAMETER_H__

#include <hkl/hkl-interval.h>

HKL_BEGIN_DECLS

typedef struct _HklParameter HklParameter;

struct _HklParameter {
	const char *name;
	HklInterval range;
	double value;
	int not_to_fit;
};

extern int hkl_parameter_init(HklParameter *parameter, char const *name, double min, double value, double max, int not_to_fit);

extern void hkl_parameter_randomize(HklParameter *parameter);

HKL_END_DECLS

#endif /* __HKL_PARAMETER_H__ */
