#ifndef __HKL_UNIT_H__
#define __HKL_UNIT_H__

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef enum _HklUnitType HklUnitType;
typedef struct _HklUnit HklUnit;

enum _HklUnitType
{
	HKL_UNIT_ANGLE_DEG,
	HKL_UNIT_ANGLE_RAD,
	HKL_UNIT_LENGTH_NM,
};

struct _HklUnit
{
	HklUnitType type;
	char const *name;
	char const *repr;
};

static HklUnit const hkl_unit_angle_deg = {HKL_UNIT_ANGLE_DEG, "Degree", "°"};
static HklUnit const hkl_unit_angle_rad = {HKL_UNIT_ANGLE_RAD, "Radian", ""};
static HklUnit const hkl_unit_length_nm = {HKL_UNIT_LENGTH_NM, "Nano Meter", "nm"};

extern int hkl_unit_compatible(HklUnit const *self, HklUnit const *unit);

extern double hkl_unit_factor(HklUnit const *self, HklUnit const *unit);

HKL_END_DECLS

#endif /* __HKL_UNIT_H__ */
