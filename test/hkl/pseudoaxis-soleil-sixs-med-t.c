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

#define SET_AXES(geometry, beta, mu, omega, gamma, delta, eta_a) do{	\
		hkl_geometry_set_values_v(geometry, 6,			\
					  beta * HKL_DEGTORAD,		\
					  mu * HKL_DEGTORAD,		\
					  omega * HKL_DEGTORAD,		\
					  gamma * HKL_DEGTORAD,		\
					  delta * HKL_DEGTORAD,		\
					  eta_a * HKL_DEGTORAD);	\
	} while(0)


static void qper_qpar(void)
{
	HklEngineList *engines;
	HklEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *Qper, *Qpar;
	double gamma;
	HklParameterList *pseudo_axes;
	const HklGeometryList *geometries;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
        hkl_sample_set_U_from_euler(sample, -90.0 * HKL_DEGTORAD, 0., 0.);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "qper_qpar");
	pseudo_axes = hkl_engine_pseudo_axes(engine);

	Qper = &darray_item(*pseudo_axes, 0)->_value;
	Qpar = &darray_item(*pseudo_axes, 1)->_value;

	/* the init part */
	SET_AXES(geom, 0., 0.1, 0., 0., 90., 0.);
	hkl_engine_initialize(engine, NULL);

	/* gamma must be positif */
	*Qper = 0.1;
	*Qpar = 4.;
	if(hkl_engine_set(engine, NULL) == HKL_TRUE){
		HklGeometryListItem *item;

		item = list_top(&geometries->items, HklGeometryListItem, node);
		gamma = hkl_parameter_get_value_unit(&item->geometry->axes[3].parameter);
		is_double(2.61077, gamma, HKL_EPSILON * 10, __func__);
	}

	/* gamma must be negatif */
	*Qper = -0.1;
	*Qpar = 4.;
	if(hkl_engine_set(engine, NULL) == HKL_TRUE){
		HklGeometryListItem *item;

		item = list_top(&geometries->items, HklGeometryListItem, node);
		gamma = hkl_parameter_get_value_unit(&item->geometry->axes[3].parameter);
		is_double(-2.7956354, gamma, HKL_EPSILON * 10, __func__);
	}

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(2);

	qper_qpar();

	return 0;
}
