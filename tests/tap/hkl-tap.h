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
#ifndef __HKL_TAP_H__
#define __HKL_TAP_H__

#include "hkl.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

/* BEWARE here we are using a GCC extension */
#define DIAG(_success)							\
	({	typeof(_success) __success = (_success);		\
		if(!__success)						\
			diag("status: %d " __FILE__ ":" TOSTRING(__LINE__) ":%s", (__success) , __func__); \
		__success;						\
	})

G_BEGIN_DECLS

extern void is_quaternion(const HklQuaternion *wanted, const HklQuaternion *seen, const char *format, ...)
	__attribute__((__format__(printf, 3, 4)));

extern void is_matrix(const HklMatrix *wanted, const HklMatrix *seen, const char *format, ...)
	__attribute__((__format__(printf, 3, 4)));

extern int check_pseudoaxes_v(HklEngine *engine, ...);

extern int check_pseudoaxes(HklEngine *engine,
			    double expected[], uint len);

extern  void hkl_tap_engine_pseudo_axes_randomize(HklEngine *self,
						  double values[], size_t n_values,
						  HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2);

extern void hkl_tap_engine_parameters_randomize(HklEngine *self) HKL_ARG_NONNULL(1);

extern HklGeometryList *hkl_engine_set_values_v(HklEngine *self, ...);

G_END_DECLS

#endif /* __HKL_TAP_H__ */
