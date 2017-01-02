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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PSEUDOAXIS_COMMON_PSI_PRIVATE_H__
#define __HKL_PSEUDOAXIS_COMMON_PSI_PRIVATE_H__

#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include "hkl-pseudoaxis-auto-private.h"  // for HklFunction, etc
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, _HklMode
#include "hkl-vector-private.h"         // for HklVector
#include "hkl.h"                        // for HklEngine, HklMode, etc

G_BEGIN_DECLS

typedef struct _HklModePsi HklModePsi;
typedef struct _HklEnginePsi HklEnginePsi;

struct _HklModePsi
{
	HklMode parent;
	HklVector Q0;
	HklVector hkl0;
};

struct _HklEnginePsi
{
	HklEngine engine;
	HklParameter *psi;
};

extern HklMode *hkl_mode_psi_new(
	const HklModeAutoInfo *info);

extern HklEngine *hkl_engine_psi_new(HklEngineList *engines);

extern int _psi_func(const gsl_vector *x, void *params, gsl_vector *f);

static const HklFunction psi_func = {
	.function = _psi_func,
	.size = 4,
};

static const HklParameter psi_parameters[] = {
	{
		HKL_PARAMETER_DEFAULTS, .name = "h2", ._value = 1,
		.description = "h coordinate of the reference plan",
		.range = { .min=-1, .max=1 },
	},
	{
		HKL_PARAMETER_DEFAULTS, .name = "k2", ._value = 1,
		.description = "k coordinate of the reference plan",
		.range = { .min=-1, .max=1 },
	},
	{
		HKL_PARAMETER_DEFAULTS, .name = "l2", ._value = 1,
		.description = "l coordinate of the reference plan",
		.range = { .min=-1, .max=1 },
	},
};

G_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_PSI_PRIVATE_H__ */
