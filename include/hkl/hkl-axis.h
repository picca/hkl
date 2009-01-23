#ifndef __HKL_AXIS_H__
#define __HKL_AXIS_H__

#include <hkl/hkl-macros.h>
#include <hkl/hkl-interval.h>
#include <hkl/hkl-vector.h>
#include <hkl/hkl-quaternion.h>
#include <hkl/hkl-parameter.h>

HKL_BEGIN_DECLS

typedef struct _HklAxis HklAxis;

struct _HklAxis {
	HklParameter parent;
	HklVector axis_v;
};

/***********/
/* HklAxis */
/**********/

extern HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v);

extern void hkl_axis_free(HklAxis *self);

/** 
 * @brief get the quaternion of an axis.
 * 
 * @param axis 
 * @param q 
 *
 * inline to speed computation.
 */
extern void hkl_axis_get_quaternion(HklAxis const *self, HklQuaternion *q);

HKL_END_DECLS

#endif /* __HKL_AXIS_H__ */
