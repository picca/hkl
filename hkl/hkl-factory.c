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
#include "hkl-factory-private.h"        // for autodata_factories_, etc

HklFactory **hkl_factory_get_all(size_t *n)
{
	return autodata_get(factories, n);
}

HklFactory *hkl_factory_get_by_name(const char *name, GError **error)
{
	size_t i, n;
	HklFactory **factories;

	factories = autodata_get(factories, &n);
	for(i=0;i<n; ++i)
		if (!strcmp(name, factories[i]->name))
			return factories[i];

	return NULL;
}

const char *hkl_factory_name_get(const HklFactory *self)
{
	return self->name;
}

HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self)
{
	return self->create_new_geometry(self);
}

HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self)
{
	return self->create_new_engine_list(self);
}
