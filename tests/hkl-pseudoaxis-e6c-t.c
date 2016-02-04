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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */

#define CHECK_AXIS_VALUE(geometry, axis, value) fabs((value) - hkl_parameter_value_get(hkl_geometry_axis_get(geometry, axis, NULL), HKL_UNIT_DEFAULT)) < HKL_EPSILON


static int hkl_geometry_list_check_geometry_unit(const HklGeometryList *self,
						 double mu,
						 double omega,
						 double chi,
						 double phi,
						 double gamma,
						 double delta)
{
	const HklGeometryListItem *item;
	int res = TRUE;

	HKL_GEOMETRY_LIST_FOREACH(item, self){
		const HklGeometry *geometry;

		geometry = hkl_geometry_list_item_geometry_get(item);

		res = TRUE;
		res &= CHECK_AXIS_VALUE(geometry, "mu", mu * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(geometry, "omega", omega * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(geometry, "chi", chi * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(geometry, "phi", phi * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(geometry, "gamma", gamma * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(geometry, "delta", delta * HKL_DEGTORAD);

		if (res)
			break;
	}
	return res;
}

static void getter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* geometry -> pseudo */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., 0., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., 0., 1.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., 90., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., -90., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, -1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., 180., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., 0., -1.));

	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 45., 0., 135., 0., 90.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., 0., -1.));

	ok(res == TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void degenerated(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 0, 1};

	factory = hkl_factory_get_by_name("E6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes) {
		const darray_string *parameters;
		HklGeometryList *geometries;
		size_t n_params;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		parameters = hkl_engine_parameters_names_get(engine);
		n_params = darray_size(*parameters);
		if (n_params){
			double params[n_params];

			hkl_engine_parameters_values_get(engine, params, n_params, HKL_UNIT_DEFAULT);
			params[0] = 0;
			res &= DIAG(hkl_engine_parameters_values_set(engine, params, n_params, HKL_UNIT_DEFAULT, NULL));
		}

		/* studdy this degenerated case */
		geometries = hkl_engine_pseudo_axis_values_set(engine,
								hkl, ARRAY_SIZE(hkl),
								HKL_UNIT_DEFAULT, NULL);
		if (geometries){
			const HklGeometryListItem *item;

			HKL_GEOMETRY_LIST_FOREACH(item, geometries){
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(item));
				res &= DIAG(check_pseudoaxes(engine, hkl, 3));
			}
			hkl_geometry_list_free(geometries);
		}
	}

	ok(res == TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void q2(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const char **mode;
	const darray_string *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "q2", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., 0., 0., 60.));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	darray_foreach(mode, *modes){
		double q, alpha;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};
				HklGeometryList *geometries;

				geometries = hkl_engine_pseudo_axis_values_set(engine,
										values, ARRAY_SIZE(values),
										HKL_UNIT_DEFAULT, NULL);
				if(geometries){
					const HklGeometryListItem *item;

					HKL_GEOMETRY_LIST_FOREACH(item, geometries){
						hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
						res &= DIAG(check_pseudoaxes(engine, values, 2));
					}
					hkl_geometry_list_free(geometries);
				}
			}
	}

	ok(res == TRUE, "q2");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void petra3(void)
{
	static double hkl_v[] = {1, 1, 0};
	static double hkl_p[] = {0, 0, 1, 90 * HKL_DEGTORAD};
	static double psi_p[] = {0, 0, 1};
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklEngine *psi;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	HklMatrix *U;

	factory = hkl_factory_get_by_name("E6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	res &= DIAG(hkl_geometry_wavelength_set(geometry, 2.033, HKL_UNIT_DEFAULT, NULL));

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(7.813, 7.813, 7.813,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

	U = hkl_matrix_new_euler(-112.5 * HKL_DEGTORAD,
				 -87.84 * HKL_DEGTORAD,
				 157.48 * HKL_DEGTORAD);
	hkl_sample_U_set(sample, U, NULL);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	/* set the psi pseudo axis */
	psi = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);
	/* set the mode parameters 0, 0, 1 */
	res &= DIAG(hkl_engine_parameters_values_set(psi, psi_p, ARRAY_SIZE(psi_p), HKL_UNIT_DEFAULT, NULL));

	/* Compute the hkl [1, 1, 0] in psi_constant_vertical mode with */
	/* h2,k2,l2= [0, 0,1] and psi = 90 */
	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	res &= DIAG(hkl_engine_current_mode_set(hkl, "psi_constant_vertical", NULL));
	/* set the mode parameters 0, 0, 1, 90. */
	res &= DIAG(hkl_engine_parameters_values_set(hkl, hkl_p, ARRAY_SIZE(hkl_p), HKL_UNIT_DEFAULT, NULL));
	geometries = hkl_engine_pseudo_axis_values_set(hkl, hkl_v, ARRAY_SIZE(hkl_v),
							HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			double PSI = hkl_p[3];
			double current;

			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(hkl_engine_initialized_set(psi, TRUE, NULL));
			res &= DIAG(hkl_engine_pseudo_axis_values_get(psi, &current, 1,
								      HKL_UNIT_DEFAULT, NULL));
			res &= DIAG(fabs(PSI - current) < HKL_EPSILON);
		}
		hkl_geometry_list_free(geometries);
	}

	ok(res == TRUE, "petra3");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

/* Problem observed with the psi_constant_vertical mode */
static void petra3_2(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklEngine *psi;
	const HklFactory *factory;
	const HklGeometryListItem *item;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	HklMatrix *U;
	double PSI;
	double hkl_p[4];
	double psi_p[3];

	/* Wavelength 1.0332035 */
	/* Mode       psi_constant_vertical */
	/* UB11  0.000 UB12  1.232 UB13  0.000  */
	/* UB21  0.000 UB22 -0.000 UB23  1.232  */
	/* UB31  1.232 UB32 -0.000 UB33 -0.000  */
	/* Ux -90 Uy 6.84979352816457e-15 Uz -90  */
	/* A 5.1 B 5.1 C 5.1  */
	/* Alpha 90 Beta 90 Gamma 90  */

	factory = hkl_factory_get_by_name("E6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	res &= DIAG(hkl_geometry_wavelength_set(geometry, 1.0332035, HKL_UNIT_DEFAULT, NULL));

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(5.1, 5.1, 5.1,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);
	U = hkl_matrix_new_euler(-90.0 * HKL_DEGTORAD,
				 0.0 * HKL_DEGTORAD,
				 -90.0 * HKL_DEGTORAD);
	hkl_sample_U_set(sample, U, NULL);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	res &= DIAG(hkl_engine_current_mode_set(hkl, "psi_constant_vertical", NULL));
	/* set the psi pseudo engine to read the psi value */
	psi = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);

	/* PsiRef 0 1 0 */
	/* freeze 0; ca 0 0 2 */
	/* for hkl */
	hkl_p[0] = 0;
	hkl_p[1] = 1;
	hkl_p[2] = 0;
	hkl_p[3] = PSI = 0;
	res &= DIAG(hkl_engine_parameters_values_set(hkl, hkl_p, ARRAY_SIZE(hkl_p), HKL_UNIT_DEFAULT, NULL));
	/* for psi */
	psi_p[0] = 0;
	psi_p[1] = 1;
	psi_p[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(psi, psi_p, ARRAY_SIZE(psi_p), HKL_UNIT_DEFAULT, NULL));

	/* freeze 0; ca 0 0 2 */
	geometries = hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37668         11.68835         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* the -90 value of phi is problematic, the right value is 0 */
	if(geometries){
		res &= DIAG(hkl_geometry_list_check_geometry_unit(geometries, 0., 11.688393153063114, 90., 0., 0., 23.376786185344031));

		/* check that all solution gives the right psi */
		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			double current;

			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(hkl_engine_initialized_set(psi, TRUE, NULL));
			res &= DIAG(hkl_engine_pseudo_axis_values_get(psi, &current, 1,
								      HKL_UNIT_DEFAULT, NULL));
			res &= DIAG(fabs(PSI - current) < HKL_EPSILON);
		}
		hkl_geometry_list_free(geometries);
	}

	/* freeze 45; ca 0 0 2 */
	hkl_p[3] = PSI = 45.0 * HKL_DEGTORAD;
	res &= DIAG(hkl_engine_parameters_values_set(hkl, hkl_p, ARRAY_SIZE(hkl_p), HKL_UNIT_DEFAULT, NULL));
	geometries = hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.3768          11.68831         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* -135 n'est pas bon il faudrait plutôt -45 */
	if(geometries){
		res &= DIAG(hkl_geometry_list_check_geometry_unit(geometries, 0., 11.688393153063114, 90., -45., 0., 23.376786185344031));

		/* check that all solution gives the right psi */
		HKL_GEOMETRY_LIST_FOREACH(item, geometries) {
			double current;

			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(hkl_engine_initialized_set(psi, TRUE, NULL));
			res &= DIAG(hkl_engine_pseudo_axis_values_get(psi, &current, 1,
								      HKL_UNIT_DEFAULT, NULL));
			res &= DIAG(fabs(PSI - current) < HKL_EPSILON);
		}
		hkl_geometry_list_free(geometries);
	}

	/* PsiRef 1 1 0 */
	/* freeze 0; ca 0 0 2 */
	hkl_p[0] = 1;
	hkl_p[1] = 1;
	hkl_p[2] = 0;
	hkl_p[3] = PSI = 0;
	/* for hkl */
	res &= DIAG(hkl_engine_parameters_values_set(hkl, hkl_p, ARRAY_SIZE(hkl_p), HKL_UNIT_DEFAULT, NULL));
	geometries = hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/* for psi */
	psi_p[0] = 1;
	psi_p[1] = 1;
	psi_p[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(psi, psi_p, ARRAY_SIZE(psi_p), HKL_UNIT_DEFAULT, NULL));

	/*      del              th               chi              phi */
	/*      23.37681         11.68839         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -45 */
	if(geometries){
		res &= DIAG(hkl_geometry_list_check_geometry_unit(geometries, 0, 11.688393153063114, 90, -45, 0, 23.376786185344031));

		/* check that all solution gives the right psi */
		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			double current;

			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(hkl_engine_initialized_set(psi, TRUE, NULL));
			res &= DIAG(hkl_engine_pseudo_axis_values_get(psi, &current, 1,
								      HKL_UNIT_DEFAULT, NULL));
			res &= DIAG(fabs(PSI - current) < HKL_EPSILON);
		}
		hkl_geometry_list_free(geometries);
	}

	/* freeze 45; ca 0 0 2 */
	hkl_p[3] = PSI = 45 * HKL_DEGTORAD;
	/* for hkl */
	res &= DIAG(hkl_engine_parameters_values_set(hkl, hkl_p, ARRAY_SIZE(hkl_p), HKL_UNIT_DEFAULT, NULL));
	geometries = hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37666         11.68837         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -90 */
	if(geometries){
		res &= DIAG(hkl_geometry_list_check_geometry_unit(geometries, 0, 11.688393153063114, 90, -90, 0, 23.376786185344031));

		/* check that all solution gives the right psi */
		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			double current;

			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(hkl_engine_initialized_set(psi, TRUE, NULL));
			res &= DIAG(hkl_engine_pseudo_axis_values_get(psi, &current, 1,
								      HKL_UNIT_DEFAULT, NULL));
			res &= DIAG(fabs(PSI - current) < HKL_EPSILON);
		}
		hkl_geometry_list_free(geometries);
	}

	ok(res == TRUE, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

int main(void)
{
	plan(5);

	getter();
	degenerated();
	q2();
	petra3();
	petra3_2();

	return 0;
}
