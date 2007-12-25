#ifndef __HKL_DIFFRACTOMETER_H__
#define __HKL_DIFFRACTOMETER_H__

#include <stdarg.h>

#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

typedef struct _HklDiffractometer HklDiffractometer;

struct _HklDiffractometer {
	HklGeometry *geometry;
};

typedef enum
{
	HKL_DIFFRACTOMETER_2C,
	HKL_DIFFRACTOMETER_E4CV,
	HKL_DIFFRACTOMETER_E6C,
	HKL_DIFFRACTOMETER_K4CV,
	HKL_DIFFRACTOMETER_K6C,
} HklDiffractometerType;

/* TODO test */
extern HklDiffractometer* hkl_diffractometer_new(HklDiffractometerType type, ...);

/* TODO test */
extern void hkl_diffractometer_free(HklDiffractometer *diffractometer);

HKL_END_DECLS

#endif /* __HKL_DIFFRACTOMETER_H__ */
