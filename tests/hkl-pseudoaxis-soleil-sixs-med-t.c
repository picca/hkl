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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */

#define GET_GAMMA(geometries) hkl_parameter_value_get(			\
		hkl_geometry_axis_get(					\
			hkl_geometry_list_item_geometry_get(		\
				hkl_geometry_list_items_first_get((geometries))), \
			"gamma", NULL), HKL_UNIT_USER)

static void qper_qpar(void)
{
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double qper_qpar[2];
	double gamma;
	HklGeometryList *geometries;
	HklMatrix *U;

	factory = hkl_factory_get_by_name("SOLEIL SIXS MED2+3", NULL);
	geom = hkl_factory_create_new_geometry(factory);

	sample = hkl_sample_new("test");
	U = hkl_matrix_new_euler(-90.0 * HKL_DEGTORAD, 0., 0.);
	hkl_sample_U_set(sample, U);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "qper_qpar", NULL);

	/* the init part */
	hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 0., 0.1, 0., 0., 90., 0.);
	hkl_engine_initialized_set(engine, TRUE, NULL);

	/* gamma must be positif */
	qper_qpar[0] = 0.1;
	qper_qpar[1] = 4.;
	geometries = hkl_engine_pseudo_axes_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar),
							HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		gamma = GET_GAMMA(geometries);
		is_double(2.61077, gamma, HKL_EPSILON * 10, __func__);
		hkl_geometry_list_free(geometries);
	}

	/* gamma must be negatif */
	qper_qpar[0] = -0.1;
	qper_qpar[1] = 4.;
	geometries = hkl_engine_pseudo_axes_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar),
							HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		gamma = GET_GAMMA(geometries);
		is_double(-2.7956354, gamma, HKL_EPSILON * 10, __func__);
		hkl_geometry_list_free(geometries);
	}

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void med_2_3(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	const HklFactory *factory;
	const HklGeometryListItem *item;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	HklMatrix *U;
	static double positions[] = {0, 1, -14.27, 99.62, 60.98, 0};
	static double hkl_p[] = {1.95, 2, 6};

	/* Wavelength 1.54980 */
	/* Mode       mu_fixed */
	/* Ux -90.59 Uy -9.97 Uz 176.35  */
	/* A 4.759 B 4.759 C 12.992  */
	/* Alpha 90 Beta 90 Gamma 120  */

	factory = hkl_factory_get_by_name("SOLEIL SIXS MED2+3", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	hkl_geometry_axes_values_set(geometry,
				     positions, ARRAY_SIZE(positions), HKL_UNIT_USER,
				     NULL);
	hkl_geometry_wavelength_set(geometry, 1.54980, HKL_UNIT_DEFAULT, NULL);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(4.759, 4.759, 12.992,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 120*HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);
	U = hkl_matrix_new_euler(-90.59 * HKL_DEGTORAD,
				 -9.97 * HKL_DEGTORAD,
				 176.35 * HKL_DEGTORAD);
	hkl_sample_U_set(sample, U);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	hkl = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	hkl_engine_current_mode_set(hkl, "mu_fixed", NULL);

	/* hkl 1.95, 2, 6 (should not fail) */
	geometries = hkl_engine_pseudo_axes_values_set(hkl,
						       hkl_p, ARRAY_SIZE(hkl_p),
						       HKL_UNIT_DEFAULT, NULL);
	res &= DIAG((geometries != NULL));
	hkl_geometry_list_free(geometries);

	ok(res == TRUE, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

int main(int argc, char** argv)
{
	plan(3);

	qper_qpar();
	med_2_3();

	return 0;
}
