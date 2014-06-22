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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

static void solution(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	static double hkl[] = {1, 1, 0};
	HklMatrix *U;

	/* get the geometry and set the source */
	factory = hkl_factory_get_by_name("ZAXIS", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	hkl_geometry_wavelength_set(geometry, 0.842, HKL_UNIT_DEFAULT, NULL);

	/* set up the sample */
	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(5.432, 5.432, 5.432,
				  90 * HKL_DEGTORAD,
				  90 * HKL_DEGTORAD,
				  90 * HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	U = hkl_matrix_new_euler(-90*HKL_DEGTORAD, 0, 0);
	hkl_sample_U_set(sample, U);
	hkl_lattice_free(lattice);
	hkl_matrix_free(U);

	/* use a 0D detector */
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	/* select the hkl pseudo axis */
	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);
	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* the init part must succed */
	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 1., 0., 0., 0.);

	/* compute the 1 1 0 */
	hkl_engine_pseudo_axes_values_set(engine, hkl, ARRAY_SIZE(hkl),
					  HKL_UNIT_DEFAULT, NULL);
	if (hkl_engine_set(engine, NULL)){
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			static double null[] = {0, 0, 0};

			hkl_engine_pseudo_axes_values_set(engine, null, ARRAY_SIZE(null),
							  HKL_UNIT_DEFAULT, NULL);
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, hkl, 3);
		}
	}else
		res = FALSE;
	if(!res)
		hkl_engine_fprintf(stdout, engine);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);

	ok(res == TRUE, "solution");
}

int main(int argc, char** argv)
{
	plan(1);

	solution();

	return 0;
}
