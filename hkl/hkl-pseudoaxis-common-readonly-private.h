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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 */
#ifndef __HKL_PSEUDOAXIS_COMMON_READONLY_PRIVATE__
#define __HKL_PSEUDOAXIS_COMMON_READONLY_PRIVATE__

#include "hkl-pseudoaxis-private.h"

typedef struct _HklModeIncidence HklModeIncidence;

struct _HklModeIncidence
{
	HklMode parent;
	HklParameter *n_x;
	HklParameter *n_y;
	HklParameter *n_z;
};

extern HklMode *hkl_mode_incidence_new(const HklModeInfo *info);

extern HklEngine *hkl_engine_incidence_new(HklEngineList *engines);

static inline double _incidence(const HklVector *v1, const HklVector *v2)
{
	return hkl_vector_angle(v1, v2) - M_PI_2;
}

/* mode parameters */

#define SURFACE_PARAMETERS(_x, _y, _z)					\
	{								\
		HKL_PARAMETER_DEFAULTS, .name = "x", ._value = (_x),	\
			.description = "the x coordinate of the surface $\\vec{n}$", \
			.range = { .min=-1, .max=1 },			\
			},						\
	{								\
		HKL_PARAMETER_DEFAULTS, .name = "y", ._value = (_y),	\
			.description = "the y coordinate of the surface $\\vec{n}$", \
			.range = { .min=-1, .max=1 },			\
			},						\
	{								\
		HKL_PARAMETER_DEFAULTS, .name = "z", ._value = (_z),	\
			.description = "the z coordinate of the surface $\\vec{n}$", \
			.range = { .min=-1, .max=1 },			\
			}

static const HklParameter surface_parameters[] = {
	SURFACE_PARAMETERS(0, 1, 0),
};

#define HKL_MODE_INFO_INCIDENCE_DEFAULTS(_name, _axes)			\
	HKL_MODE_INFO_RO_WITH_PARAMS((_name), (_axes), surface_parameters)

#define REGISTER_INCIDENCE_ENGINE(_name)				\
	static HklEngine *hkl_engine_## _name ## _incidence_new(HklEngineList *engines) \
	{								\
		HklEngine *self;					\
		HklMode *default_mode;					\
		static const HklModeInfo info = {			\
			HKL_MODE_INFO_INCIDENCE_DEFAULTS("incidence",	\
							 _name ## _incidence_axes), \
		};							\
		self = hkl_engine_incidence_new(engines);		\
		default_mode = hkl_mode_incidence_new(&info);		\
		hkl_engine_add_mode(self, default_mode);		\
		hkl_engine_mode_set(self, default_mode);		\
		return self;						\
	}
#endif
