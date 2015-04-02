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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_AXIS_PRIVATE_H__
#define __HKL_AXIS_PRIVATE_H__

#include "hkl-parameter-private.h"      // for _HklParameter
#include "hkl-quaternion-private.h"     // for _HklQuaternion
#include "hkl-vector-private.h"         // for HklVector, HklQuaternion
#include "hkl.h"                        // for HklParameter, etc

G_BEGIN_DECLS

typedef struct _HklAxis HklAxis;

struct _HklAxis {
	HklParameter parameter;
	HklVector axis_v;
	HklQuaternion q;
};

extern HklParameter *hkl_parameter_new_axis(char const *name, HklVector const *axis_v);

G_END_DECLS

#endif /* __HKL_AXIS_PRIVATE_H__ */
