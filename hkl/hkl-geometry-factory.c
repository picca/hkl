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

#include <stdarg.h>

#include <hkl/hkl-geometry-factory.h>

static void hkl_geometry_init_twoC_vertical(HklGeometry *self,
					    const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_eulerian4C_vertical(HklGeometry *self,
						  const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_kappa4C_vertical(HklGeometry *self,
					       const HklGeometryConfig *config, double alpha)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_eulerian6C(HklGeometry *self,
					 const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}

static void hkl_geometry_init_kappa6C(HklGeometry *self,
				      const  HklGeometryConfig *config, double alpha)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}

static void hkl_geometry_init_zaxis(HklGeometry *self,
				    const HklGeometryConfig *config)
{
 	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1); 
}

const HklGeometryConfig *hkl_geometry_factory_get_config_from_type(HklGeometryType type)
{
	const HklGeometryConfig *config;

	config = hkl_geometry_factory_configs;
	while(config)
		if(config->type == type)
			return config;
		else
			config++;
	return NULL;
}


HklGeometry *hkl_geometry_factory_new(const HklGeometryConfig *config, ...)
{
	HklGeometry *geom;
	double alpha;
	va_list ap;

	geom = hkl_geometry_new();
	switch(config->type) {
		case HKL_GEOMETRY_TYPE_TWOC_VERTICAL:
			hkl_geometry_init_twoC_vertical(geom, config);
			break;
		case HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL:
			hkl_geometry_init_eulerian4C_vertical(geom, config);
			break;
		case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:
			va_start(ap, config);
			alpha = va_arg(ap, double);
			va_end(ap);
			hkl_geometry_init_kappa4C_vertical(geom, config, alpha);
			break;
		case HKL_GEOMETRY_TYPE_EULERIAN6C:
			hkl_geometry_init_eulerian6C(geom, config);
			break;
		case HKL_GEOMETRY_TYPE_KAPPA6C:
			va_start(ap, config);
			alpha = va_arg(ap, double);
			va_end(ap);
			hkl_geometry_init_kappa6C(geom, config, alpha);
			break;
		case HKL_GEOMETRY_TYPE_ZAXIS:
			hkl_geometry_init_zaxis(geom, config);
			break;
	}

	return geom;
}
