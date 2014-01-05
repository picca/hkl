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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_TAP_H__
#define __HKL_TAP_H__

#include "hkl.h"

HKL_BEGIN_DECLS

extern int check_pseudoaxes_v(HklEngine *engine, ...);

extern int check_pseudoaxes(HklEngine *engine,
			    double expected[], uint len);

extern void hkl_engine_set_values_v(HklEngine *self, ...);

HKL_END_DECLS

#endif /* __HKL_TAP_H__ */
