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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-macros-private.h"
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_list_add, etc
#include "hkl/ccan/array_size/array_size.h"

#define OMEGA "omega"
#define TTH "tth"

#define HKL_GEOMETRY_TWOC_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 1 axes for the sample\n"					\
	"\n"								\
	"  + **" OMEGA "** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **" TTH "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_twoC_axes[] = {OMEGA, TTH};

static HklGeometry *hkl_geometry_new_twoC(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, TTH, 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_twoC(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	return self;
}

REGISTER_DIFFRACTOMETER(twoC, "TwoC", HKL_GEOMETRY_TWOC_DESCRIPTION);
