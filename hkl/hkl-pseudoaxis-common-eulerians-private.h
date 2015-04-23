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
#ifndef __HKL_PSEUDOAXIS_COMMON_EULERIANS_H__
#define __HKL_PSEUDOAXIS_COMMON_EULERIANS_H__

#include "hkl.h"                        // for HklParameter, etc

G_BEGIN_DECLS

extern HklEngine *hkl_engine_eulerians_new(HklEngineList *engines);

extern void kappa_2_kappap(double komega, double kappa, double kphi, double alpha,
			   double *komegap, double *kappap, double *kphip);

G_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_EULERIANS_H__ */
