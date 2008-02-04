#ifndef __HKL_HOLDER_H__
#define __HKL_HOLDER_H__

#include <hkl/hkl-axes.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;

struct _HklHolder {
	HklAxes *axes;
	HklAxes *private_axes;
};

extern HklHolder *hkl_holder_new(HklAxes *axes);

extern void hkl_holder_free(HklHolder *holder);

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *holder,
		char const *name, double x, double y, double z);

HKL_END_DECLS

#endif /* _NEW_HOLDER_H */
