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
#include <hkl.h>
#include <tap/basic.h>
#include <tap/hkl.h>

static void solution(void)
{
	int res = 0;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	double h, k, l;

	/* get the geometry and set the source */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_ZAXIS);
	geometry = hkl_geometry_factory_new(config);
	hkl_source_init(&geometry->source, 0.842, 1, 0, 0);

	/* set up the sample */
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       5.432, 5.432, 5.432,
			       90 * HKL_DEGTORAD,
			       90 * HKL_DEGTORAD,
			       90 * HKL_DEGTORAD);
	hkl_sample_set_U_from_euler(sample, -90*HKL_DEGTORAD, 0, 0);

	/* use a 0D detector */
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	/* select the hkl pseudo axis */
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	/* the init part must succed */
	hkl_geometry_set_values_unit_v(geometry, 1., 0., 0., 0.);

	/* compute the 1 1 0 */
	h = k = 1;
	l = 0;
	hkl_parameter_set_value(&engine->pseudoAxes[0]->parent, h);
	hkl_parameter_set_value(&engine->pseudoAxes[1]->parent, k);
	hkl_parameter_set_value(&engine->pseudoAxes[2]->parent, l);

	if (hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS){
		int i;

		for(i=0; i<engines->geometries->len; ++i){
			hkl_geometry_init_geometry(geometry,
						   engines->geometries->items[i].geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			res |= check_pseudoaxes(engine, h, k, l);
		}
	}else
		res = -1;

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);

	ok(res == 0, "solution");
}

int main(int argc, char** argv)
{
	plan(1);

	solution();

	return 0;
}
