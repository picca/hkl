#ifndef __HKL_AXIS_H__
#define __HKL_AXIS_H__

#include <hkl/hklmacros.h>
#include <hkl/hklinterval.h>
#include <hkl/hklvector.h>

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

extern void hkl_axis_init(HklAxis *axis, char const *name, HklVector const *axis_v);

HKL_END_DECLS

#endif /* __HKL_AXIS_H__ */
