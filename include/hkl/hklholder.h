#ifndef __HKL_HOLDER_H__
#define __HKL_HOLDER_H__

#include <hkl/hklaxes.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;

struct _HklHolder {
	HklAxes * axes;
	unsigned int len;
	unsigned int alloc;
	HklAxis **private_axes;
};

extern void hkl_holder_init(HklHolder *holder, HklAxes *axes);

extern void hkl_holder_release(HklHolder *holder);

extern HklAxis* hkl_holder_add_rotation_axis(HklHolder *holder, char const *name, double x, double y, double z);

HKL_END_DECLS

#endif /* _NEW_HOLDER_H */
