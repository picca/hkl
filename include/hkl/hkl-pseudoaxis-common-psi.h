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
#ifndef __HKL_PSEUDOAXIS_COMMON_PSI_H__
#define __HKL_PSEUDOAXIS_COMMON_PSI_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxisEngineModePsi HklPseudoAxisEngineModePsi;

struct _HklPseudoAxisEngineModePsi
{
	HklPseudoAxisEngineMode parent;
	HklVector Q0;
	HklVector hkl0;
};

extern HklPseudoAxisEngineModePsi *hkl_pseudo_axis_engine_mode_psi_new(char const *name,
									    size_t axes_names_len,
									    char const *axes_names[]);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_psi_new(void);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_PSI_H__ */
