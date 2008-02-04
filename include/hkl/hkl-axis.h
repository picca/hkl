#ifndef __HKL_AXIS_H__
#define __HKL_AXIS_H__

#include <hkl/hkl-macros.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-vector.h>

HKL_BEGIN_DECLS

typedef struct _HklAxis HklAxis;
typedef struct _HklAxisConfig HklAxisConfig;

struct _HklAxisConfig {
	HklInterval range;
	double current;
	double consign;
};

struct _HklAxis {
	const char * name;
	HklVector axis_v;
	HklAxisConfig config;
};

extern HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v);

extern void hkl_axis_free(HklAxis *axis);

HKL_END_DECLS

#endif /* __HKL_AXIS_H__ */
