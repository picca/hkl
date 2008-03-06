#ifndef __HKL_DETECTOR_H__
#define __HKL_DETECTOR_H__

#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

typedef struct _HklDetector HklDetector;

struct _HklDetector
{
	size_t idx;
};

extern HklDetector *hkl_detector_new(void);
extern HklDetector *hkl_detector_new_copy(HklDetector const *src);

extern void hkl_detector_free(HklDetector *d);

extern int hkl_detector_get_kf(HklDetector const *d, HklGeometry *g,
		HklVector *kf, HklVector *kfc);

HKL_END_DECLS

#endif /* __HKL_DETECTOR_H__ */
