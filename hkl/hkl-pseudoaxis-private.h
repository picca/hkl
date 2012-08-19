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
#ifndef __HKL_PSEUDOAXIS_PRIVATE_H__
#define __HKL_PSEUDOAXIS_PRIVATE_H__

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

/**************************************/
/* to move in hkl-engine-auto-private */
/**************************************/

struct _HklFunction
{
	const uint size;
	int (* function) (const gsl_vector *x, void *params, gsl_vector *f);
};

#define CHECK_NAN(x, len) do{				\
		for(uint i=0; i<len; ++i)		\
			if(gsl_isnan(x[i]))		\
				return GSL_ENOMEM;	\
	}while(0)

/*************/
/* stay here */
/*************/

static inline void set_geometry_axes(HklPseudoAxisEngine *engine, const double values[])
{
	HklAxis *axis;
	uint i = 0;

	list_for_each(&engine->axes, axis, engine_list)
		hkl_axis_set_value(axis, values[i++]);
	hkl_geometry_update(engine->geometry);
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_PRIVATE_H__ */
