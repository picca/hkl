#ifndef __HKL_DETECTOR_H__
#define __HKL_DETECTOR_H__

#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

typedef struct _HklDetector HklDetector;

struct _HklDetector
{
	size_t idx;
};

extern int hkl_detector_compute_kf(HklDetector const *self, HklGeometry *g,
		HklVector *kf);

HKL_END_DECLS

#endif /* __HKL_DETECTOR_H__ */
