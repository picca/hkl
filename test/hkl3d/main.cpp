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
#include <string.h>

#include "hkl3d.h"
#include "tap/basic.h"

#define MODEL_FILENAME "data/diffabs.yaml"

int main(int argc, char** argv)
{
	char* filename;
	const HklGeometryConfig *config;
	HklGeometry *geometry;
	int i, j, len;
	int res;
	struct Hkl3DObject *obji;
	struct Hkl3DObject *objj;
	struct Hkl3D *hkl3d;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, HKL_DEGTORAD * 50.);

	/* compute the filename of the diffractometer config file */
	filename  = test_file_path(MODEL_FILENAME);

	plan(5);

	hkl3d = hkl3d_new(filename, geometry);

	hkl3d_configs_fprintf(stdout, hkl3d->configs);

	// collision
	hkl_geometry_set_values_v(geometry, 6,
				  45 * HKL_DEGTORAD, 0., 0., 0., 0., 0.);
	ok(hkl3d_is_colliding(hkl3d) == true, "collision");

	// no-collision
	hkl_geometry_set_values_v(geometry, 6,
				  0., 0., 0., 0., 0., 0.);
	ok(hkl3d_is_colliding(hkl3d) == false, "no-collision");

	// imported 1 config files with 7 Hkl3DObjects
	ok(hkl3d->configs->len == 1, "configs len");
	ok(hkl3d->configs->configs[0].len == 7, "objects len");

	// all Hkl3DObjects must have a different axis_name
	len = hkl3d->configs->configs[0].len;
	res = false;
	obji = &hkl3d->configs->configs[0].objects[0];
	for(i=0;i<len; ++i){
		for (j=1; j<len-i; ++j){
			objj = obji + j;
			if(!(strcmp(obji->axis_name, objj->axis_name))){
				res = TRUE;
				break;
			}
		}
		obji++;
	}
	ok(res == FALSE, "no identical objects");
	
	test_file_path_free(filename);
	hkl3d_free(hkl3d);
	hkl_geometry_free(geometry);

	return 0;
}
