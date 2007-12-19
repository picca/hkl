#ifndef __HKL_AXES_H__
#define __HKL_AXES_H__

#include <stdlib.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklAxes HklAxes;

struct _HklAxes {
	size_t alloc;
	size_t len;
	HklAxis *axes;
};

/* TODO test */
extern HklAxes* hkl_axes_new(void);

extern void hkl_axes_init(HklAxes *axes);

extern void hkl_axes_release(HklAxes *axes);

/* TODO test */
extern void hkl_axes_free(HklAxes *axes);

extern HklAxis * hkl_axes_add_rotation(HklAxes *axes, char const *name, HklVector const *axis_v);

HKL_END_DECLS

#endif /* __HKL_AXES_H__ */
