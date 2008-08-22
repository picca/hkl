#ifndef __HKL_AXIS_H__
#define __HKL_AXIS_H__

#include <hkl/hkl-macros.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-vector.h>
#include <hkl/hkl-quaternion.h>

HKL_BEGIN_DECLS

typedef struct _HklAxis HklAxis;
typedef struct _HklAxisConfig HklAxisConfig;

struct _HklAxisConfig {
	HklInterval range;
	double value;
	int dirty;
};

struct _HklAxis {
	const char * name;
	HklVector axis_v;
	HklAxisConfig config;
};

extern void hkl_axis_init(HklAxis *axis, char const *name, HklVector const *axis_v);

extern void hkl_axis_get_config(HklAxis *axis, HklAxisConfig *config);
extern void hkl_axis_set_config(HklAxis *axis, HklAxisConfig *config);

extern void hkl_axis_clear_dirty(HklAxis *axis);

extern void hkl_axis_get_quaternion(HklAxis const *axis, HklQuaternion *q);

HKL_END_DECLS

#endif /* __HKL_AXIS_H__ */
