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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */

#define CHECK_AXIS_VALUE(axis, value) fabs((value) - hkl_parameter_value_get(axis)) < HKL_EPSILON


static int hkl_geometry_list_check_geometry_unit(const HklGeometryList *self,
						 double mu,
						 double omega,
						 double chi,
						 double phi,
						 double gamma,
						 double delta)
{
	const darray_item *items = hkl_geometry_list_items_get(self);
	HklGeometryListItem **item;
	int res = HKL_TRUE;

	darray_foreach(item, *items){
		const darray_parameter *axes = hkl_geometry_axes_get(hkl_geometry_list_item_geometry_get(*item));

		res = HKL_TRUE;
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 0), mu * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 1), omega * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 2), chi * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 3), phi * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 4), gamma * HKL_DEGTORAD);
		res &= CHECK_AXIS_VALUE(darray_item(*axes, 5), delta * HKL_DEGTORAD);

		if (res)
			break;
	}
	return res;
}

static void getter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E6C");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "hkl");

	/* geometry -> pseudo */
	hkl_geometry_set_values_unit_v(geometry, 0., 30., 0., 0., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 0., 1.);

	hkl_geometry_set_values_unit_v(geometry, 0., 30., 0., 90., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., 0.);

	hkl_geometry_set_values_unit_v(geometry, 0., 30., 0., -90., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, -1., 0., 0.);

	hkl_geometry_set_values_unit_v(geometry, 0., 30., 0., 180., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 0., -1.);

	hkl_geometry_set_values_unit_v(geometry, 0., 45., 0., 135., 0., 90.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., -1.);

	ok(res == HKL_TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void degenerated(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 0, 1};
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E6C");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "hkl");
	modes = hkl_engine_modes_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	darray_foreach(mode, *modes) {
		darray_parameter *parameters;

		hkl_engine_select_mode(engine, *mode);
		parameters = hkl_mode_parameters_get(*mode);
		if (darray_size(*parameters))
			hkl_parameter_value_set(darray_item(*parameters, 0), 0, NULL);

		/* studdy this degenerated case */
		hkl_parameter_list_values_set(pseudo_axes,
					      hkl, ARRAY_SIZE(hkl),
					      NULL);
		if (hkl_engine_set(engine, NULL)){
			const darray_item *items = hkl_geometry_list_items_get(geometries);
			HklGeometryListItem **item;

			darray_foreach(item, *items) {
				static double null[] = {0, 0, 0};

				hkl_parameter_list_values_set(pseudo_axes,
							      null, ARRAY_SIZE(null),
							      NULL);
				hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
				hkl_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, hkl, 3);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void q2(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E6C");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "q2");
	modes = hkl_engine_modes_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	/* the init part */
	hkl_geometry_set_values_unit_v(geometry, 0., 30., 0., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);


	darray_foreach(mode, *modes){
		double q, alpha;

		hkl_engine_select_mode(engine, *mode);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};

				hkl_parameter_list_values_set(pseudo_axes,
							      values, ARRAY_SIZE(values),
							      NULL);
				if(hkl_engine_set(engine, NULL)){
					const darray_item *items = hkl_geometry_list_items_get(geometries);
					HklGeometryListItem **item;

					darray_foreach(item, *items){
						static double null[] = {0, 0};

						hkl_parameter_list_values_set(pseudo_axes,
									      null, ARRAY_SIZE(null),
									      NULL);
						hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
						hkl_engine_get(engine, NULL);
						res &= check_pseudoaxes(engine, values, 2);
					}
				}
			}
	}

	ok(res == HKL_TRUE, "q2");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void petra3(void)
{
	static double values[] = {1, 1, 0};
	static double parameters[] = {0, 0, 1, 90 * HKL_DEGTORAD};
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklEngine *psi;
	HklMode *mode;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	HklMatrix *U;

	factory = hkl_factory_get_by_name("E6C");
	geometry = hkl_factory_create_new_geometry(factory);
	hkl_geometry_wavelength_set(geometry, 2.033);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(7.813, 7.813, 7.813,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

	U = hkl_matrix_new_euler(-112.5 * HKL_DEGTORAD,
				 -87.84 * HKL_DEGTORAD,
				 157.48 * HKL_DEGTORAD);
	hkl_sample_U_set(sample, U);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_engine_list_get_by_name(engines, "hkl");
	hkl_engine_select_mode_by_name(hkl, "psi_constant_vertical");
	hkl_engine_set_values_v(hkl, 1, 1, 0);
	/* set the mode parameters 0, 0, 1, 90. */
	mode = hkl_engine_mode_get(hkl);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);

	/* set the psi pseudo axis */
	psi = hkl_engine_list_get_by_name(engines, "psi");
	hkl_engine_select_mode_by_name(psi, "psi_constant_vertical");
	/* set the mode parameters 0, 0, 1 */
	mode = hkl_engine_mode_get(psi);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters), NULL);

	/* Compute the hkl [1, 1, 0] in psi_constant_vertical mode with */
	/* h2,k2,l2= [0, 0,1] and psi = 90 */
	if(hkl_engine_set(hkl, NULL)){
		const darray_item *items = hkl_geometry_list_items_get(geometries);
		HklGeometryListItem **item;
		darray_parameter *pseudo_axes = hkl_engine_pseudo_axes_get(psi);

		darray_foreach(item, *items) {
			double PSI = parameters[3];

			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_initialize(psi, NULL);
			hkl_engine_list_get(engines);
			res &= fabs(PSI - hkl_parameter_value_get(darray_item(*pseudo_axes, 0))) < HKL_EPSILON;
		}
	}

	ok(res == HKL_TRUE, "petra3");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

/* Problem observed with the psi_constant_vertical mode */
static void petra3_2(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *hkl;
	HklEngine *psi;
	HklMode *mode;
	const HklFactory *factory;
	const darray_item *items;
	HklGeometryListItem **item;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklLattice *lattice;
	HklMatrix *U;
	double PSI;
	double parameters[4];
	darray_parameter *pseudo_axes;

	/* Wavelength 1.0332035 */
	/* Mode       psi_constant_vertical */
	/* UB11  0.000 UB12  1.232 UB13  0.000  */
	/* UB21  0.000 UB22 -0.000 UB23  1.232  */
	/* UB31  1.232 UB32 -0.000 UB33 -0.000  */
	/* Ux -90 Uy 6.84979352816457e-15 Uz -90  */
	/* A 5.1 B 5.1 C 5.1  */
	/* Alpha 90 Beta 90 Gamma 90  */

	factory = hkl_factory_get_by_name("E6C");
	geometry = hkl_factory_create_new_geometry(factory);
	hkl_geometry_wavelength_set(geometry, 1.0332035);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(5.1, 5.1, 5.1,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);
	U = hkl_matrix_new_euler(-90.0 * HKL_DEGTORAD,
				 0.0 * HKL_DEGTORAD,
				 -90.0 * HKL_DEGTORAD);
	hkl_sample_U_set(sample, U);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries(engines);
	items = hkl_geometry_list_items_get(geometries);

	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_engine_list_get_by_name(engines, "hkl");
	hkl_engine_select_mode_by_name(hkl, "psi_constant_vertical");
	/* set the psi pseudo engine to read the psi value */
	psi = hkl_engine_list_get_by_name(engines, "psi");
	pseudo_axes = hkl_engine_pseudo_axes_get(psi);

	/* PsiRef 0 1 0 */
	/* freeze 0; ca 0 0 2 */
	/* for hkl */
	parameters[0] = 0;
	parameters[1] = 1;
	parameters[2] = 0;
	parameters[3] = PSI = 0;
	mode = hkl_engine_mode_get(hkl);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);
	/* for psi */
	mode = hkl_engine_mode_get(psi);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);

	/* freeze 0; ca 0 0 2 */
	hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37668         11.68835         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* the -90 value of phi is problematic, the right value is 0 */
	if(hkl_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			geometries,
			0., 11.688393153063114, 90., 0., 0.,  23.376786185344031);

		/* check that all solution gives the right psi */
		darray_foreach(item, *items) {
			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_initialize(psi, NULL);
			hkl_engine_list_get(engines);
			res &= fabs(PSI - hkl_parameter_value_get(darray_item(*pseudo_axes, 0))) < HKL_EPSILON;
		}
	}

	/* freeze 45; ca 0 0 2 */
	parameters[3] = PSI = 45.0 * HKL_DEGTORAD;
	mode = hkl_engine_mode_get(hkl);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters), NULL);
	hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.3768          11.68831         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* -135 n'est pas bon il faudrait plutôt -45 */
	if(hkl_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			geometries,
			0., 11.688393153063114, 90., -45., 0.,  23.376786185344031);

		/* check that all solution gives the right psi */
		darray_foreach(item, *items) {
			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_initialize(psi, NULL);
			hkl_engine_list_get(engines);
			res &= fabs(PSI - hkl_parameter_value_get(darray_item(*pseudo_axes, 0))) < HKL_EPSILON;
		}
	}

	/* PsiRef 1 1 0 */
	/* freeze 0; ca 0 0 2 */
	parameters[0] = 1;
	parameters[1] = 1;
	parameters[2] = 0;
	parameters[3] = PSI = 0;
	/* for hkl */
	mode = hkl_engine_mode_get(hkl);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);
	hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/* for psi */
	mode = hkl_engine_mode_get(psi);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);

	/*      del              th               chi              phi */
	/*      23.37681         11.68839         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -45 */
	if(hkl_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			geometries,
			0, 11.688393153063114, 90, -45, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		darray_foreach(item, *items) {
			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_initialize(psi, NULL);
			hkl_engine_list_get(engines);
			res &= fabs(PSI - hkl_parameter_value_get(darray_item(*pseudo_axes, 0))) < HKL_EPSILON;
		}
	}

	/* freeze 45; ca 0 0 2 */
	parameters[3] = PSI = 45 * HKL_DEGTORAD;
	/* for hkl */
	mode = hkl_engine_mode_get(hkl);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      parameters, ARRAY_SIZE(parameters),
				      NULL);
	hkl_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37666         11.68837         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -90 */
	if(hkl_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			geometries,
			0, 11.688393153063114, 90, -90, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		darray_foreach(item, *items){
			hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_initialize(psi, NULL);
			hkl_engine_list_get(engines);
			res &= fabs(PSI - hkl_parameter_value_get(darray_item(*pseudo_axes, 0))) < HKL_EPSILON;
		}
	}

	ok(res == HKL_TRUE, __func__);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

int main(int argc, char** argv)
{
	plan(5);

	getter();
	degenerated();
	q2();
	petra3();
	petra3_2();

	return 0;
}
