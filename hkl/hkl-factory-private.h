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
 */
#ifndef __HKL_FACTORY_PRIVATE_H__
#define __HKL_FACTORY_PRIVATE_H__

#include "hkl.h"
#include "hkl/ccan/autodata/autodata.h"

G_BEGIN_DECLS

typedef HklGeometry* (* HklFactoryGeometryFunction) (const HklFactory *factory);
typedef HklEngineList* (* HklFactoryEngineListFunction) (const HklFactory *factory);

struct _HklFactory
{
	const char *name;
	const char *description;
	const darray_string axes;
	HklFactoryGeometryFunction create_new_geometry;
	HklFactoryEngineListFunction create_new_engine_list;
};

#define REGISTER_DIFFRACTOMETER(name_, real_name_, description_)	\
	static HklFactory name_ = {					\
		.name = real_name_,					\
		.description = description_,				\
		.axes = DARRAY(hkl_geometry_ ## name_ ## _axes),	\
		.create_new_geometry = &hkl_geometry_new_ ## name_,	\
		.create_new_engine_list = &hkl_engine_list_new_ ## name_ \
	};								\
	AUTODATA(factories, &name_)

AUTODATA_TYPE(factories, HklFactory);

G_END_DECLS

#endif /* __HKL_FACTORY_H__ */
