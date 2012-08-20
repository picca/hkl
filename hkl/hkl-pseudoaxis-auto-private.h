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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PSEUDOAXIS_AUTO_H__
#define __HKL_PSEUDOAXIS_AUTO_H__

#include <gsl/gsl_vector.h>

#include "hkl-pseudoaxis-private.h"

HKL_BEGIN_DECLS

typedef struct _HklFunction HklFunction;
typedef struct _HklPseudoAxisEngineModeAutoInfo HklPseudoAxisEngineModeAutoInfo;

struct _HklFunction
{
	const uint size;
	int (* function) (const gsl_vector *x, void *params, gsl_vector *f);
};

struct _HklPseudoAxisEngineModeAutoInfo {
	const HklPseudoAxisEngineModeInfo mode;
	const HklFunction **functions;
	const uint n_functions;
};

#define CHECK_NAN(x, len) do{				\
		for(uint i=0; i<len; ++i)		\
			if(gsl_isnan(x[i]))		\
				return GSL_ENOMEM;	\
	}while(0)

#define INFO_AUTO(name, axes, fn) .mode={INFO(name, axes),}, .functions=fn, .n_functions=ARRAY_SIZE(fn)
#define INFO_AUTO_WITH_PARAMS(name, axes, fn, parameters) .mode={INFO_WITH_PARAMS(name, axes, parameters)}, .functions=fn, .n_functions=ARRAY_SIZE(fn)

extern HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_auto_new(const HklPseudoAxisEngineModeAutoInfo *info,
								     const HklPseudoAxisEngineModeOperations *ops);

void hkl_pseudo_axis_engine_mode_auto_init(HklPseudoAxisEngineMode *self,
					   const HklPseudoAxisEngineModeAutoInfo *info,
					   const HklPseudoAxisEngineModeOperations *ops);

extern int hkl_pseudo_axis_engine_mode_set_real(HklPseudoAxisEngineMode *self,
						HklPseudoAxisEngine *engine,
						HklGeometry *geometry,
						HklDetector *detector,
						HklSample *sample,
						HklError **error);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_AUTO_H__ */
