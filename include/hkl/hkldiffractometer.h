#ifndef __HKL_DIFFRACTOMETER_H__
#define __HKL_DIFFRACTOMETER_H__

#include <stdarg.h>

#include <hkl/hklgeometry.h>

HKL_BEGIN_DECLS

typedef struct _HklDiffractometer HklDiffractometer;

struct _HklDiffractometer {
	HklGeometry geometry;
};

typedef enum
{
	HKL_DIFFRACTOMETER_2C,
	HKL_DIFFRACTOMETER_E4CV,
	HKL_DIFFRACTOMETER_E6C,
	HKL_DIFFRACTOMETER_K4CV,
	HKL_DIFFRACTOMETER_K6C,
} HklDiffractometerType;

extern void hkl_diffractometer_init(HklDiffractometer *diffractometer, HklDiffractometerType type, ...);

extern void hkl_diffractometer_release(HklDiffractometer *diffractometer);

HKL_END_DECLS

#endif /* __HKL_DIFFRACTOMETER_H__ */
