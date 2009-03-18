/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_UNIT_H__
#define __HKL_UNIT_H__

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklUnit HklUnit;

enum _HklUnitType
{
	HKL_UNIT_ANGLE_DEG,
	HKL_UNIT_ANGLE_RAD,
	HKL_UNIT_LENGTH_NM,
};

typedef enum _HklUnitType HklUnitType;

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
