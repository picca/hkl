#ifndef __HKL_AXES_H__
#define __HKL_AXES_H__

#include <stdlib.h>
#include <hkl/hkl-list.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklAxes HklAxes;

struct _HklAxes {
	HklList *axes;
};

extern HklAxes* hkl_axes_new(void);

extern void hkl_axes_free(HklAxes *axes);

extern HklAxis * hkl_axes_add_rotation(HklAxes *axes,
		char const *name, HklVector const *axis_v);

HKL_END_DECLS

#endif /* __HKL_AXES_H__ */
