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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_sf_trig.h>            // for gsl_sf_angle_restrict_symm
#include <math.h>                       // for cos, sin, M_PI_2, atan, tan
#include <string.h>                     // for NULL, strcmp
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-geometry-private.h"
#include "hkl-pseudoaxis-common-eulerians-private.h"
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_list_add, etc
#include "hkl-pseudoaxis-zaxis-private.h"  // for hkl_engine_zaxis_hkl_new
#include "hkl.h"                        // for HklFactory, HklGeometry, etc
#include "hkl/ccan/autodata/autodata.h"  // for AUTODATA, autodata_get
#include "hkl/ccan/darray/darray.h"     // for darray_item

HklFactory **hkl_factory_get_all(unsigned int *n)
{
	return autodata_get(factories, n);
}

HklFactory *hkl_factory_get_by_name(const char *name, GError **error)
{
	unsigned int i, n;
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

/*********/
/* ZAXIS */
/*********/

#define HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION				\
	"For this geometry the **mu** axis is common to the sample and the detector.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

static const char* hkl_geometry_zaxis_axes[] = {"mu", "omega", "delta", "gamma"};

static HklGeometry *hkl_geometry_new_zaxis(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);

	return self;
}

static HklEngineList *hkl_engine_list_new_zaxis(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_zaxis_hkl_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(zaxis, "ZAXIS", HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION);
