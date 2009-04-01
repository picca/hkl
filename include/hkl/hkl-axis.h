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
	HklQuaternion q;
};

/***********/
/* HklAxis */
/**********/

extern HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v);

extern void hkl_axis_free(HklAxis *self);

extern void hkl_axis_init(HklAxis *axis, char const * name, HklVector const *axis_v);

extern double hkl_axis_get_value_unit(HklAxis const *self);

extern void hkl_axis_set_value(HklAxis *self, double value);

extern void hkl_axis_set_value_unit(HklAxis *self, double value);

extern void hkl_axis_randomize(HklAxis *self);

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
