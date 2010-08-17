/* This file is part of the hkl3d library.
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
 * Copyright (C) 2010      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Oussama Sboui <sboui@synchrotron-soleil.fr>
 */

#include "hkl3d.h"
#include "tap/basic.h"

#define MODEL_FILENAME "data/diffabs.yaml"

int main(int argc, char** argv)
{
	char* filename;
	const HklGeometryConfig *config;
	HklGeometry *geometry;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, HKL_DEGTORAD * 50.);

	/* compute the filename of the diffractometer config file */
	filename  = test_file_path(MODEL_FILENAME);
	plan(2);
	try{
		Hkl3D hkl3d(filename, geometry);

		// collision
		hkl_geometry_set_values_v(geometry, 6,
					  45 * HKL_DEGTORAD, 0., 0., 0., 0., 0.);
		ok(hkl3d.is_colliding() == true, "collision");

		// no-collision
		hkl_geometry_set_values_v(geometry, 6,
					  0., 0., 0., 0., 0., 0.);
		ok(hkl3d.is_colliding() == false, "no-collision");

	} catch(...)
	{}

	test_file_path_free(filename);
	hkl_geometry_free(geometry);

	return 0;
}
