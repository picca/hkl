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


typedef HklGeometry* (* HklFactoryGeometryFunction) (HklFactory *self);
typedef HklEngineList* (* HklFactoryEngineListFunction) (HklFactory *self);

struct _HklFactory
{
	const HklGeometryConfig config;
	HklFactoryGeometryFunction create_new_geometry;
	HklFactoryEngineListFunction create_new_engine_list;
};

const char *hkl_factory_name(const HklFactory *self)
{
	return self->config.name;
}

HklGeometry *hkl_factory_create_new_geometry(HklFactory *self)
{
	return self->create_new_geometry(self);
}

HklEngineList *hkl_factory_create_new_engine_list(HklFactory *self)
{
	return self->create_new_engine_list(self);
}

#define CREATE_CONSTRUCTOR(name, type, description)			\
	static HklGeometry *create_new_geometry_ ## name (HklFactory *factory) \
	{								\
		HklGeometry *geometry;					\
		geometry = hkl_geometry_new();				\
		hkl_geometry_init_ ## name (geometry, &factory->config); \
			return geometry;				\
	}

#define CREATE_CONSTRUCTOR_KAPPA(name, type, description, alpha)	\
	static HklGeometry *create_new_geometry_ ## name (HklFactory *factory) \
	{								\
		HklGeometry *geometry;					\
		geometry = hkl_geometry_new();				\
		hkl_geometry_init_ ## name (geometry, &factory->config, alpha); \
			return geometry;				\
	}

#define CREATE_ENGINE_LIST_CONSTRUCTOR(name) \
	static HklEngineList *create_new_engine_list_ ## name (HklFactory *factory) \
	{								\
		return hkl_engine_list_factory(&factory->config);	\
	}
	
#define REGISTER_DIFFRACTOMETER(name_, real_name_, type_, description_)	\
	CREATE_CONSTRUCTOR(name_, type_, description_);			\
	CREATE_ENGINE_LIST_CONSTRUCTOR(name_);				\
	static HklFactory name_ = {.config = {.name = real_name_, .type = type_,	.description = description_}, \
					    .create_new_geometry = &create_new_geometry_ ## name_, \
					    .create_new_engine_list = &create_new_engine_list_ ## name_ \
	};\
	AUTODATA(factories, &name_)

#define REGISTER_DIFFRACTOMETER_KAPPA(name_, real_name_, type_, description_, alpha) \
	CREATE_CONSTRUCTOR_KAPPA(name_, type_, description_, alpha);	\
	CREATE_ENGINE_LIST_CONSTRUCTOR(name_);				\
	static HklFactory name_ = {.config = {.name = real_name_, .type = type_,	.description = description_}, \
					    .create_new_geometry = &create_new_geometry_ ## name_, \
					    .create_new_engine_list = &create_new_engine_list_ ## name_ \
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
	komega = hkl_parameter_get_value(&geometry->axes[0].parameter);
	kappa = hkl_parameter_get_value(&geometry->axes[1].parameter);
	kphi = hkl_parameter_get_value(&geometry->axes[2].parameter);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	/* TODO parameter list for the geometry */
	hkl_parameter_set_value(&copy->axes[0].parameter, komegap, NULL);
	hkl_parameter_set_value(&copy->axes[1].parameter, kappap, NULL);
	hkl_parameter_set_value(&copy->axes[2].parameter, kphip, NULL);

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
	komega = hkl_parameter_get_value(&geometry->axes[1].parameter);
	kappa = hkl_parameter_get_value(&geometry->axes[2].parameter);
	kphi = hkl_parameter_get_value(&geometry->axes[3].parameter);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	/* TODO parameter list for the geometry */
	hkl_parameter_set_value(&copy->axes[1].parameter, komegap, NULL);
	hkl_parameter_set_value(&copy->axes[2].parameter, kappap, NULL);
	hkl_parameter_set_value(&copy->axes[3].parameter, kphip, NULL);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}

/**
 * hkl_engine_list_factory:
 * @config:
 *
 * create an #HklEngineList given an #HklGeometryConfig
 *
 * Returns: (transfer full):
 **/
HklEngineList *hkl_engine_list_factory(const HklGeometryConfig *config)
{
	HklEngineList *self = NULL;

	self = hkl_engine_list_new();

	switch(config->type){
	case HKL_GEOMETRY_TYPE_TWOC_VERTICAL:
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL:
	case HKL_GEOMETRY_TYPE_SOLEIL_MARS:
	case HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL:
		hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
		hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
		hkl_engine_list_add(self, hkl_engine_q_new());
		break;
	case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:
		self->geometries->multiply = hkl_geometry_list_multiply_k4c_real;
		hkl_engine_list_add(self, hkl_engine_k4cv_hkl_new());
		hkl_engine_list_add(self, hkl_engine_eulerians_new());
		hkl_engine_list_add(self, hkl_engine_k4cv_psi_new());
		hkl_engine_list_add(self, hkl_engine_q_new());
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN6C:
		hkl_engine_list_add(self, hkl_engine_e6c_hkl_new());
		hkl_engine_list_add(self, hkl_engine_e6c_psi_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	case HKL_GEOMETRY_TYPE_KAPPA6C:
		self->geometries->multiply = hkl_geometry_list_multiply_k6c_real;
		hkl_engine_list_add(self, hkl_engine_k6c_hkl_new());
		hkl_engine_list_add(self, hkl_engine_eulerians_new());
		hkl_engine_list_add(self, hkl_engine_k6c_psi_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	case HKL_GEOMETRY_TYPE_ZAXIS:
		hkl_engine_list_add(self, hkl_engine_zaxis_hkl_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2:
		hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_2_2_hkl_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2:
		hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_1_2_hkl_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	case HKL_GEOMETRY_TYPE_PETRA3_P09_EH2:
		hkl_engine_list_add(self, hkl_engine_petra3_p09_eh2_hkl_new());
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3:
		self->geometries->multiply = hkl_geometry_list_multiply_soleil_sixs_med_2_3;
		hkl_engine_list_add(self, hkl_engine_soleil_sixs_med_2_3_hkl_new());
		hkl_engine_list_add(self, hkl_engine_q2_new());
		hkl_engine_list_add(self, hkl_engine_qper_qpar_new());
		break;
	}
	return self;
}

/********/
/* TwoC */
/********/

static void hkl_geometry_init_twoC(HklGeometry *self,
				   const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

/********/
/* E4CV */
/********/

static void hkl_geometry_init_eulerian4C_vertical(HklGeometry *self,
				   const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_kappa4C_vertical(HklGeometry *self,
					       const HklGeometryConfig *config, double alpha)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_eulerian6C(HklGeometry *self,
					 const HklGeometryConfig *config)
{
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
}

static void hkl_geometry_init_kappa6C(HklGeometry *self,
				      const  HklGeometryConfig *config, double alpha)
{
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
}

static void hkl_geometry_init_zaxis(HklGeometry *self,
				    const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
}

static void hkl_geometry_init_soleil_sixs_med_2_2(HklGeometry *self,
						  const HklGeometryConfig *config)
{
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
}

static void hkl_geometry_init_soleil_mars(HklGeometry *self,
					  const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", -1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_soleil_sixs_med_1_2(HklGeometry *self,
						  const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "pitch", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}

static void hkl_geometry_init_petra3_p09_eh2(HklGeometry *self,
					     const HklGeometryConfig *config)
{
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
}

static void hkl_geometry_init_soleil_sixs_med_2_3(HklGeometry *self,
						  const HklGeometryConfig *config)
{
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
}

static void hkl_geometry_init_eulerian4C_horizontal(HklGeometry *self,
						    const HklGeometryConfig *config)
{
	HklHolder *h;

	self->config = config;
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, 0, 1);
}

/**
 * hkl_geometry_factory_get_config_from_type:
 * @type:
 *
 * get am #HklGeometryConfig for a given #HklGeometryType
 *
 * Returns: (transfer none):
 **/
const HklGeometryConfig *hkl_geometry_factory_get_config_from_type(HklGeometryType type)
{
	const HklGeometryConfig *config;

	config = hkl_geometry_factory_configs;
	while(config)
		if(config->type == type)
			return config;
		else
			config++;
	return NULL;
}


/**
 * hkl_geometry_factory_new: (skip)
 * @config:
 * @...:
 *
 * create an #HklGeometry given an #HklGeometryConfig
 *
 * Returns: (transfer full): a new #HklGeometry
 **/
HklGeometry *hkl_geometry_factory_new(const HklGeometryConfig *config, ...)
{
	HklGeometry *geometry;
	double *parameters = NULL;
	int len = 0;
	va_list ap;

	switch(config->type) {
	case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:
	case HKL_GEOMETRY_TYPE_KAPPA6C:
		parameters = malloc(1 * sizeof(*parameters));
		va_start(ap, config);
		parameters[0] = va_arg(ap, double);
		va_end(ap);
		break;
	default:
		break;
	}
	geometry = hkl_geometry_factory_newv(config, parameters, len);
	if(parameters)
		free(parameters);

	return geometry;
}

/**
 * hkl_geometry_factory_newv:
 * @config:
 * @parameters: (array length=len):
 * @len:
 *
 * factory constructor
 *
 * Returns: (transfer full): a new HklGeometry
 **/
HklGeometry *hkl_geometry_factory_newv(const HklGeometryConfig *config,
				       const double parameters[], const int len)
{
	HklGeometry *geometry;
	double alpha;

	geometry = hkl_geometry_new();
	switch(config->type) {
	case HKL_GEOMETRY_TYPE_TWOC_VERTICAL:
		hkl_geometry_init_twoC(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL:
		hkl_geometry_init_eulerian4C_vertical(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:
		alpha = parameters[0];
		hkl_geometry_init_kappa4C_vertical(geometry, config, alpha);
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN6C:
		hkl_geometry_init_eulerian6C(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_KAPPA6C:
		alpha = parameters[0];
		hkl_geometry_init_kappa6C(geometry, config, alpha);
		break;
	case HKL_GEOMETRY_TYPE_ZAXIS:
		hkl_geometry_init_zaxis(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2:
		hkl_geometry_init_soleil_sixs_med_2_2(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_MARS:
		hkl_geometry_init_soleil_mars(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2:
		hkl_geometry_init_soleil_sixs_med_1_2(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_PETRA3_P09_EH2:
		hkl_geometry_init_petra3_p09_eh2(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3:
		hkl_geometry_init_soleil_sixs_med_2_3(geometry, config);
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL:
		hkl_geometry_init_eulerian4C_horizontal(geometry, config);
		break;
	}
	return geometry;
}

REGISTER_DIFFRACTOMETER(twoC, "TwoC", HKL_GEOMETRY_TYPE_TWOC_VERTICAL, HKL_GEOMETRY_TWOC_DESCRIPTION);
REGISTER_DIFFRACTOMETER(eulerian4C_vertical, "E4CV", HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL, HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION);
REGISTER_DIFFRACTOMETER_KAPPA(kappa4C_vertical, "K4CV", HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL, HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION, 50 * HKL_DEGTORAD);
REGISTER_DIFFRACTOMETER(eulerian6C, "E6C", HKL_GEOMETRY_TYPE_EULERIAN6C, HKL_GEOMETRY_EULERIAN6C_DESCRIPTION);
REGISTER_DIFFRACTOMETER_KAPPA(kappa6C, "K6C", HKL_GEOMETRY_TYPE_KAPPA6C, HKL_GEOMETRY_KAPPA6C_DESCRIPTION, 50 * HKL_DEGTORAD);
REGISTER_DIFFRACTOMETER(zaxis, "ZAXIS", HKL_GEOMETRY_TYPE_ZAXIS, HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION);
REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_2,"SOLEIL SIXS MED2+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION);
REGISTER_DIFFRACTOMETER(soleil_mars, "SOLEIL MARS", HKL_GEOMETRY_TYPE_SOLEIL_MARS, HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION);
REGISTER_DIFFRACTOMETER(soleil_sixs_med_1_2, "SOLEIL SIXS MED1+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION);
REGISTER_DIFFRACTOMETER(petra3_p09_eh2, "PETRA3 P09 EH2", HKL_GEOMETRY_TYPE_PETRA3_P09_EH2, HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION);
REGISTER_DIFFRACTOMETER(soleil_sixs_med_2_3, "SOLEIL SIXS MED2+3", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION);
REGISTER_DIFFRACTOMETER(eulerian4C_horizontal, "E4CH", HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL, HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION);
