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

#include <stdarg.h>
#include <gsl/gsl_sf.h>
#include <hkl/hkl-factory-private.h>

#include <hkl/hkl-pseudoaxis-private.h>
#include <hkl/hkl-pseudoaxis-common-eulerians-private.h>
#include <hkl/hkl-pseudoaxis-common-q-private.h>
#include <hkl/hkl-pseudoaxis-e4c-private.h>
#include <hkl/hkl-pseudoaxis-k4cv-private.h>
#include <hkl/hkl-pseudoaxis-e6c-private.h>
#include <hkl/hkl-pseudoaxis-k6c-private.h>
#include <hkl/hkl-pseudoaxis-zaxis-private.h>
#include <hkl/hkl-pseudoaxis-soleil-sixs-med-private.h>
#include <hkl/hkl-pseudoaxis-petra3-private.h>


typedef HklGeometry* (* HklFactoryGeometryFunction) (const HklGeometryConfig *config);
typedef HklEngineList* (* HklFactoryEngineListFunction) (const HklGeometryConfig *config);

struct _HklFactory
{
	const HklGeometryConfig config;
	HklFactoryGeometryFunction create_new_geometry;
	HklFactoryEngineListFunction create_new_engine_list;
};

HklFactory **hkl_factory_get_all(unsigned int *n)
{
	return autodata_get(factories, n);
}

HklFactory *hkl_factory_get_by_name(const char *name)
{
	unsigned int i, n;
	HklFactory **factories;

	factories = autodata_get(factories, &n);
	for(i=0;i<n; ++i)
		if (!strcmp(name, factories[i]->config.name))
			return factories[i];

	return NULL;
}

const char *hkl_factory_name(const HklFactory *self)
{
	return self->config.name;
}

HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self)
{
	return self->create_new_geometry(&self->config);
}

HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self)
{
	return self->create_new_engine_list(&self->config);
}

#define REGISTER_DIFFRACTOMETER(name_, real_name_, type_, description_)	\
	static HklFactory name_ = {.config = {.name = real_name_, .type = type_, .description = description_}, \
				   .create_new_geometry = &hkl_geometry_new_ ## name_, \
				   .create_new_engine_list = &hkl_engine_list_new_ ## name_ \
	};								\
	AUTODATA(factories, &name_)

static void kappa_2_kappap(double komega, double kappa, double kphi, double alpha,
			   double *komegap, double *kappap, double *kphip)
{
	double p;
	double omega;
	double phi;

	p = atan(tan(kappa/2.) * cos(alpha));
	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	*komegap = gsl_sf_angle_restrict_symm(2*omega - komega);
	*kappap = -kappa;
	*kphip = gsl_sf_angle_restrict_symm(2*phi - kphi);

}

static void hkl_geometry_list_multiply_k4c_real(HklGeometryList *self,
						HklGeometryListItem *item)
{
	HklGeometry *geometry;
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;

	geometry = item->geometry;
	komega = hkl_parameter_get_value(&darray_item(geometry->axes, 0)->parameter);
	kappa = hkl_parameter_get_value(&darray_item(geometry->axes, 1)->parameter);
	kphi = hkl_parameter_get_value(&darray_item(geometry->axes, 2)->parameter);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	/* TODO parameter list for the geometry */
	hkl_parameter_set_value(&darray_item(copy->axes, 0)->parameter, komegap, NULL);
	hkl_parameter_set_value(&darray_item(copy->axes, 1)->parameter, kappap, NULL);
	hkl_parameter_set_value(&darray_item(copy->axes, 2)->parameter, kphip, NULL);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}

static void hkl_geometry_list_multiply_k6c_real(HklGeometryList *self,
						HklGeometryListItem *item)
{
	HklGeometry *geometry;
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;

	geometry = item->geometry;
	komega = hkl_parameter_get_value(&darray_item(geometry->axes, 1)->parameter);
	kappa = hkl_parameter_get_value(&darray_item(geometry->axes, 2)->parameter);
	kphi = hkl_parameter_get_value(&darray_item(geometry->axes, 3)->parameter);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	/* TODO parameter list for the geometry */
	hkl_parameter_set_value(&darray_item(copy->axes, 1)->parameter, komegap, NULL);
	hkl_parameter_set_value(&darray_item(copy->axes, 2)->parameter, kappap, NULL);
	hkl_parameter_set_value(&darray_item(copy->axes, 3)->parameter, kphip, NULL);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}


/********/
/* TwoC */
/********/

#define HKL_GEOMETRY_TWOC_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 1 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_twoC(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_twoC(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	return self;
}

REGISTER_DIFFRACTOMETER(twoC, "TwoC", HKL_GEOMETRY_TYPE_TWOC_VERTICAL, HKL_GEOMETRY_TWOC_DESCRIPTION);

/********/
/* E4CV */
/********/
#define HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n"	\
	"  + **phi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_eulerian4C_vertical(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_vertical(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian4C_vertical, "E4CV", HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL, HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION);

/********/
/* K4CV */
/********/

#define HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION			\
	"For this geometry there is a special parameters called :math:`\\alpha` which is the\n" \
	"angle between the kappa rotation axis and the  :math:`\\vec{y}` direction.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **komega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **kappa** : rotating around the :math:`\\vec{x}` direction (0, :math:`-\\cos\\alpha`, :math:`-\\sin\\alpha`)\n"	\
	"  + **kphi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_kappa4C_vertical(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	double alpha = 50 * HKL_DEGTORAD;
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_kappa4C_vertical(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	self->geometries->multiply = hkl_geometry_list_multiply_k4c_real;
	hkl_engine_list_add(self, hkl_engine_k4cv_hkl_new());
	hkl_engine_list_add(self, hkl_engine_eulerians_new());
	hkl_engine_list_add(self, hkl_engine_k4cv_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(kappa4C_vertical, "K4CV", HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL, HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION);

/*******/
/* E6C */
/*******/

#define HKL_GEOMETRY_EULERIAN6C_DESCRIPTION				\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_eulerian6C(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian6C(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e6c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e6c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian6C, "E6C", HKL_GEOMETRY_TYPE_EULERIAN6C, HKL_GEOMETRY_EULERIAN6C_DESCRIPTION);

/*******/
/* K6C */
/*******/

#define HKL_GEOMETRY_KAPPA6C_DESCRIPTION				\
	"For this geometry there is a special parameters called :math:`\\alpha` which is the\n" \
	"angle between the kappa rotation axis and the  :math:`\\vec{y}` direction.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **komega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **kappa** : rotating around the :math:`\\vec{x}` direction (0, :math:`-\\cos\\alpha`, :math:`-\\sin\\alpha`)\n" \
	"  + **kphi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_kappa6C(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	double alpha = 50 * HKL_DEGTORAD;
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_kappa6C(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	self->geometries->multiply = hkl_geometry_list_multiply_k6c_real;
	hkl_engine_list_add(self, hkl_engine_k6c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_eulerians_new());
	hkl_engine_list_add(self, hkl_engine_k6c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(kappa6C, "K6C", HKL_GEOMETRY_TYPE_KAPPA6C, HKL_GEOMETRY_KAPPA6C_DESCRIPTION);

/*********/
/* ZAXIS */
/*********/

#define HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION				\
	"For this geometry the **mu** axis is common to the sample and the detector.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

static HklGeometry *hkl_geometry_new_zaxis(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);

	return self;
}

static HklEngineList *hkl_engine_list_new_zaxis(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_zaxis_hkl_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(zaxis, "ZAXIS", HKL_GEOMETRY_TYPE_ZAXIS, HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION);

/***********************/
/* SOLEIL SIXS MED 2+2 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_soleil_sixs_med_2_2(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_2_2(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_2_2_hkl_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_2,"SOLEIL SIXS MED2+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION);

/***************/
/* SOLEIL MARS */
/***************/

#define HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`\\vec{z}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (-1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`\\vec{z}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_soleil_mars(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", -1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_mars(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_mars, "SOLEIL MARS", HKL_GEOMETRY_TYPE_SOLEIL_MARS, HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION);

/***********************/
/* SOLEIL SIXS MED 1+2 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_soleil_sixs_med_1_2(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_1_2(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_1_2_hkl_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_1_2, "SOLEIL SIXS MED1+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION);

/******************/
/* PETRA3 P09 EH2 */
/******************/

#define HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **omega** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **delta** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **gamma** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static HklGeometry *hkl_geometry_new_petra3_p09_eh2(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "omega", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "delta", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "gamma", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_petra3_p09_eh2(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_petra3_p09_eh2_hkl_new());

	return self;
}

REGISTER_DIFFRACTOMETER(petra3_p09_eh2, "PETRA3 P09 EH2", HKL_GEOMETRY_TYPE_PETRA3_P09_EH2, HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION);

/***********************/
/* SOLEIL SIXS MED 2+3 */
/***********************/

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 4 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **eta_a** : rotation around the :math:`-\\vec{x}` direction (-1, 0, 0)\n"

static HklGeometry *hkl_geometry_new_soleil_sixs_med_2_3(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "beta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "eta_a", -1, 0, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_sixs_med_2_3(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	self->geometries->multiply = hkl_geometry_list_multiply_soleil_sixs_med_2_3;
	hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_2_3_hkl_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_3, "SOLEIL SIXS MED2+3", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION);

/********/
/* E4CH */
/********/

#define HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

static HklGeometry *hkl_geometry_new_eulerian4C_horizontal(const HklGeometryConfig *config)
{
	HklGeometry *self = hkl_geometry_new();
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, 0, 1);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_horizontal(const HklGeometryConfig *config)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian4C_horizontal, "E4CH", HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL, HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION);
