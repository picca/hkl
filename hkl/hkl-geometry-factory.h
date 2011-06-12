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
#ifndef __HKL_GEOMETRY_FACTORY_H__
#define __HKL_GEOMETRY_FACTORY_H__

#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

static const HklGeometryConfig hkl_geometry_factory_configs[] =
{
	{"TwoC", HKL_GEOMETRY_TYPE_TWOC_VERTICAL},
	{"E4CV", HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL},
	{"K4CV", HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL},
	{"E6C", HKL_GEOMETRY_TYPE_EULERIAN6C},
	{"K6C", HKL_GEOMETRY_TYPE_KAPPA6C},
	{"ZAXIS", HKL_GEOMETRY_TYPE_ZAXIS},
	{"SOLEIL SIXS MED2+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2},
	{"SOLEIL MARS", HKL_GEOMETRY_TYPE_SOLEIL_MARS},
	{NULL}
};

extern const HklGeometryConfig *hkl_geometry_factory_get_config_from_type(HklGeometryType type);

extern HklGeometry *hkl_geometry_factory_new(const HklGeometryConfig *config, ...);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_FACTORY_H__ */
