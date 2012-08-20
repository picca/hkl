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

static int hkl_geometry_list_check_geometry_unit(HklGeometryList *self,
						 double mu,
						 double omega,
						 double chi,
						 double phi,
						 double gamma,
						 double delta)
{
	HklGeometryListItem *item;
	int res;

	list_for_each(&self->items, item, node){
		HklAxis *axes;

		axes = item->geometry->axes;

		res = HKL_TRUE;
		res &= fabs(mu * HKL_DEGTORAD - ((HklParameter *)(&axes[0]))->value) < HKL_EPSILON;
		res &= fabs(omega * HKL_DEGTORAD - ((HklParameter *)(&axes[1]))->value) < HKL_EPSILON;
		res &= fabs(chi * HKL_DEGTORAD - ((HklParameter *)(&axes[2]))->value) < HKL_EPSILON;
		res &= fabs(phi * HKL_DEGTORAD - ((HklParameter *)(&axes[3]))->value) < HKL_EPSILON;
		res &= fabs(gamma * HKL_DEGTORAD - ((HklParameter *)(&axes[4]))->value) < HKL_EPSILON;
		res &= fabs(delta * HKL_DEGTORAD - ((HklParameter *)(&axes[5]))->value) < HKL_EPSILON;
		if (res)
			break;
	}
	return res;
}

static void getter(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	/* geometry -> pseudo */
	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 0., 0., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 0., 1.);

	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 90., 0., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., -90., 0., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, -1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 180., 0., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 0., -1.);

	hkl_geometry_set_values_unit_v(geom, 0., 45., 0., 135., 0., 90.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., -1.);

	ok(res == HKL_TRUE, "getter");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void degenerated(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 0, 1};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	list_for_each(&engine->modes, mode, list) {
		hkl_pseudo_axis_engine_select_mode(engine, mode);
		if (engine->mode->parameters_len)
			engine->mode->parameters[0].value = 0.;

		/* studdy this degenerated case */
		hkl_pseudo_axis_engine_set_values(engine, hkl, 3);
		if (hkl_pseudo_axis_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node) {
				static double null[] = {0, 0, 0};

				hkl_pseudo_axis_engine_set_values(engine, null, 3);
				hkl_geometry_init_geometry(geom, item->geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, hkl, 3);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q2(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "q2");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 0., 0., 60.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);


	list_for_each(&engine->modes, mode, list){
		double q, alpha;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};

				hkl_pseudo_axis_engine_set_values(engine, values, 2);
				if(hkl_pseudo_axis_engine_set(engine, NULL)){
					HklGeometryListItem *item;

					list_for_each(&engines->geometries->items, item, node){
						static double null[] = {0, 0};

						hkl_pseudo_axis_engine_set_values(engine, null, 2);
						hkl_geometry_init_geometry(geom, item->geometry);
						hkl_pseudo_axis_engine_get(engine, NULL);
						res &= check_pseudoaxes(engine, values, 2);
					}
				}
			}
	}

	ok(res == HKL_TRUE, "q2");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void petra3(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *hkl;
	HklPseudoAxisEngine *psi;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	static double values[] = {1, 1, 0};
	double PSI = 90;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geom = hkl_geometry_factory_new(config);
	hkl_source_init(&geom->source, 2.033, 1., 0., 0.);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       7.813, 7.813, 7.813,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_sample_set_U_from_euler(sample,
				    -112.5 * HKL_DEGTORAD,
				    -87.84 * HKL_DEGTORAD,
				    157.48 * HKL_DEGTORAD);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");
	hkl_pseudo_axis_engine_select_mode_by_name(hkl, "psi_constant_vertical");
	hkl_pseudo_axis_engine_set_values_v(hkl, 1, 1, 0);
	hkl_parameter_set_value_unit( &hkl->mode->parameters[0], 0); /* h2 */
	hkl_parameter_set_value_unit( &hkl->mode->parameters[1], 0); /* k2 */
	hkl_parameter_set_value_unit( &hkl->mode->parameters[2], 1); /* l2 */
	hkl_parameter_set_value_unit( &hkl->mode->parameters[3], PSI); /* psi */

	/* set the psi pseudo axis */
	psi = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");
	hkl_pseudo_axis_engine_select_mode_by_name(psi, "psi_constant_vertical");
	hkl_parameter_set_value_unit( &psi->mode->parameters[0], 0); /* h2 */
	hkl_parameter_set_value_unit( &psi->mode->parameters[1], 0); /* k2 */
	hkl_parameter_set_value_unit( &psi->mode->parameters[2], 1); /* l2 */

	/* Compute the hkl [1, 1, 0] in psi_constant_vertical mode with */
	/* h2,k2,l2= [0, 0,1] and psi = 90 */
	if(hkl_pseudo_axis_engine_set(hkl, NULL)){
		HklGeometryListItem *item;

		list_for_each(&engines->geometries->items, item, node) {
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			res &= fabs(PSI*HKL_DEGTORAD - list_top(&psi->pseudo_axes, HklPseudoAxis, list)->parent.value) < HKL_EPSILON;
		}
	}

	ok(res == HKL_TRUE, "petra3");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

/* Problem observed with the psi_constant_vertical mode */
static void petra3_2(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *hkl;
	HklPseudoAxisEngine *psi;
	const HklGeometryConfig *config;
	HklGeometryListItem *item;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	double PSI;

	/* Wavelength 1.0332035 */
	/* Mode       psi_constant_vertical */
	/* UB11  0.000 UB12  1.232 UB13  0.000  */
	/* UB21  0.000 UB22 -0.000 UB23  1.232  */
	/* UB31  1.232 UB32 -0.000 UB33 -0.000  */
	/* Ux -90 Uy 6.84979352816457e-15 Uz -90  */
	/* A 5.1 B 5.1 C 5.1  */
	/* Alpha 90 Beta 90 Gamma 90  */

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geometry = hkl_geometry_factory_new(config);
	hkl_source_init(&geometry->source, 1.0332035, 1., 0., 0.);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       5.1, 5.1, 5.1,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_sample_set_U_from_euler(sample,
				    -90.0 * HKL_DEGTORAD,
				    0.0 * HKL_DEGTORAD,
				    -90.0 * HKL_DEGTORAD);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);

	/* set the hkl pseudo axis in psi_constant_vertical */
	hkl = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");
	hkl_pseudo_axis_engine_select_mode_by_name(hkl, "psi_constant_vertical");
	/* set the psi pseudo engine to read the psi value */
	psi = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	/* PsiRef 0 1 0 */
	/* for hkl */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[0], 0); /* h2 */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[1], 1); /* k2 */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[2], 0); /* l2 */
	/* for psi */
	hkl_parameter_set_value_unit(&psi->mode->parameters[0], 0); /* h1 */
	hkl_parameter_set_value_unit(&psi->mode->parameters[1], 1); /* k1 */
	hkl_parameter_set_value_unit(&psi->mode->parameters[2], 0); /* l1 */

	/* freeze 0; ca 0 0 2 */
	PSI = 0;
	hkl_pseudo_axis_engine_set_values_v(hkl, 0., 0., 2.);
	hkl_parameter_set_value_unit(&hkl->mode->parameters[3], PSI); /* psi */

	/*      del              th               chi              phi */
	/*      23.37668         11.68835         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* the -90 value of phi is problematic, the right value is 0 */
	if(hkl_pseudo_axis_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			engines->geometries,
			0, 11.688393153063114, 90, 0, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		list_for_each(&engines->geometries->items, item, node) {
			hkl_geometry_init_geometry(geometry, item->geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			res &= fabs(PSI * HKL_DEGTORAD - list_top(&psi->pseudo_axes, HklPseudoAxis, list)->parent.value) < HKL_EPSILON;
		}
	}

	/* freeze 45; ca 0 0 2 */
	PSI = 45.0;
	hkl_parameter_set_value_unit(&hkl->mode->parameters[3], PSI); /* psi */
	hkl_pseudo_axis_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.3768          11.68831         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* -135 n'est pas bon il faudrait plutôt -45 */
	if(hkl_pseudo_axis_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			engines->geometries,
			0, 11.688393153063114, 90, -45, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		list_for_each(&engines->geometries->items, item, node) {
			hkl_geometry_init_geometry(geometry, item->geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			res &= fabs(PSI * HKL_DEGTORAD - list_top(&psi->pseudo_axes, HklPseudoAxis, list)->parent.value) < HKL_EPSILON;
		}
	}

	/* PsiRef 1 1 0 */
	/* for hkl */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[0], 1); /* h2 */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[1], 1); /* k2 */
	hkl_parameter_set_value_unit(&hkl->mode->parameters[2], 0); /* l2 */
	/* for psi */
	hkl_parameter_set_value_unit(&psi->mode->parameters[0], 1); /* h1 */
	hkl_parameter_set_value_unit(&psi->mode->parameters[1], 1); /* k1 */
	hkl_parameter_set_value_unit(&psi->mode->parameters[2], 0); /* l1 */

	/* freeze 0; ca 0 0 2 */
	PSI = 0;
	hkl_parameter_set_value_unit(&hkl->mode->parameters[3], PSI); /* psi */
	hkl_pseudo_axis_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37681         11.68839         90               -90 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -45 */
	if(hkl_pseudo_axis_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			engines->geometries,
			0, 11.688393153063114, 90, -45, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		list_for_each(&engines->geometries->items, item, node) {
			hkl_geometry_init_geometry(geometry, item->geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			res &= fabs(PSI * HKL_DEGTORAD - list_top(&psi->pseudo_axes, HklPseudoAxis, list)->parent.value) < HKL_EPSILON;
		}
	}

	/* freeze 45; ca 0 0 2 */
	PSI = 45;
	hkl_parameter_set_value_unit(&hkl->mode->parameters[3], PSI); /* psi */
	hkl_pseudo_axis_engine_set_values_v(hkl, 0., 0., 2.);

	/*      del              th               chi              phi */
	/*      23.37666         11.68837         90               -135 */
	/*      gamma            mu */
	/*      -2.384186e-09    -1.182388e-14 */
	/* phi is wrong it should be -90 */
	if(hkl_pseudo_axis_engine_set(hkl, NULL)){
		res &= hkl_geometry_list_check_geometry_unit(
			engines->geometries,
			0, 11.688393153063114, 90, -90, 0,  23.376786185344031);

		/* check that all solution gives the right psi */
		list_for_each(&engines->geometries->items, item, node) {
			hkl_geometry_init_geometry(geometry, item->geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			res &= fabs(PSI * HKL_DEGTORAD - list_top(&psi->pseudo_axes, HklPseudoAxis, list)->parent.value) < HKL_EPSILON;
		}
	}

	ok(res == HKL_TRUE, __func__);

	hkl_pseudo_axis_engine_list_free(engines);
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
