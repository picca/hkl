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

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_soleil_sixs_med

#define SET_AXES(geometry, beta, mu, omega, gamma, delta, eta_a) do{	\
		hkl_geometry_set_values_v(geometry, 6,			\
					  beta * HKL_DEGTORAD,		\
					  mu * HKL_DEGTORAD,		\
					  omega * HKL_DEGTORAD,		\
					  gamma * HKL_DEGTORAD,		\
					  delta * HKL_DEGTORAD,		\
					  eta_a * HKL_DEGTORAD);	\
	} while(0)


HKL_TEST_SUITE_FUNC(qper_qpar)
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *Qper, *Qpar;
	double gamma;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
        hkl_sample_set_U_from_euler(sample, -90.0 * HKL_DEGTORAD, 0., 0.);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "qper_qpar");

	Qper = &(((HklParameter *)engine->pseudoAxes[0])->value);
	Qpar = &(((HklParameter *)engine->pseudoAxes[1])->value);

	/* the init part */
	SET_AXES(geom, 0., 0.1, 0., 0., 90., 0.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);

	/* gamma must be positif */
	*Qper = 0.1;
	*Qpar = 4.;
	if(hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS){
		gamma = hkl_axis_get_value_unit(&engines->geometries->items[0]->geometry->axes[3]);
		HKL_ASSERT_DOUBLES_EQUAL(2.61077, gamma,
					 HKL_EPSILON * 10);
	}

	/* gamma must be negatif */
	*Qper = -0.1;
	*Qpar = 4.;
	if(hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS){
		gamma = hkl_axis_get_value_unit(&engines->geometries->items[0]->geometry->axes[3]);
		HKL_ASSERT_DOUBLES_EQUAL(-2.7956354, gamma,
					 HKL_EPSILON * 10);
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}


HKL_TEST_SUITE_BEGIN

HKL_TEST( qper_qpar );

HKL_TEST_SUITE_END
