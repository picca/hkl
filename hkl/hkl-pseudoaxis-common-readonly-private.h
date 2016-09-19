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
 * Copyright (C) 2003-2016 Synchrotron SOLEIL
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

static inline double _emergence(const HklVector *v1, const HklVector *v2)
{
	return M_PI_2 - hkl_vector_angle(v1, v2);
}

extern HklMode *hkl_mode_emergence_new(const HklModeInfo *info);

extern HklEngine *hkl_engine_emergence_new(HklEngineList *engines);

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

static const HklParameter surface_parameters_y[] = {
	SURFACE_PARAMETERS(0, 1, 0),
};

static const HklParameter surface_parameters_z[] = {
	SURFACE_PARAMETERS(0, 0, 1),
};

#define P99_PROTECT(...) __VA_ARGS__

#define HKL_MODE_INFO_incidence_DEFAULTS(_axes, _parameters)		\
	HKL_MODE_INFO_RO_WITH_PARAMS("incidence", (_axes), (_parameters))

#define HKL_MODE_INFO_emergence_DEFAULTS(_axes, _parameters)		\
	HKL_MODE_INFO_RO_WITH_PARAMS("emergence", (_axes), (_parameters))

#define REGISTER_READONLY(_engine, _func, _axes, _parameters)		\
	static HklEngine* _func(HklEngineList *engines)			\
	{								\
		HklEngine *self = hkl_engine_ ## _engine ## _new(engines); \
		static const char *axes[] = _axes;			\
		static const HklModeInfo info = {			\
			HKL_MODE_INFO_ ## _engine ## _DEFAULTS(axes, _parameters), \
		};							\
		HklMode *default_mode = hkl_mode_ ## _engine ## _new(&info); \
		hkl_engine_add_mode(self, default_mode);		\
		hkl_engine_mode_set(self, default_mode);		\
		return self;						\
	}

#define REGISTER_READONLY_INCIDENCE(_func, _axes, _parameters) REGISTER_READONLY(incidence, _func, P99_PROTECT(_axes), _parameters)
#define REGISTER_READONLY_EMERGENCE(_func, _axes, _parameters) REGISTER_READONLY(emergence, _func, P99_PROTECT(_axes), _parameters)

#endif
