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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-eulerians-private.h"
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for RUBh_minus_Q, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc

static void hkl_geometry_list_multiply_k4c_real(HklGeometryList *self,
						HklGeometryListItem *item)
{
	HklGeometry *geometry;
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;

	geometry = item->geometry;
	komega = hkl_parameter_value_get(darray_item(geometry->axes, 0), HKL_UNIT_DEFAULT);
	kappa = hkl_parameter_value_get(darray_item(geometry->axes, 1), HKL_UNIT_DEFAULT);
	kphi = hkl_parameter_value_get(darray_item(geometry->axes, 2), HKL_UNIT_DEFAULT);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	/* TODO parameter list for the geometry */
	hkl_parameter_value_set(darray_item(copy->axes, 0), komegap, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(darray_item(copy->axes, 1), kappap, HKL_UNIT_DEFAULT, NULL);
	hkl_parameter_value_set(darray_item(copy->axes, 2), kphip, HKL_UNIT_DEFAULT, NULL);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}

/************/
/* hkl mode */
/************/

/* bissector */

static int _bissector_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	const double tth = x->data[3];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_f1 = {
	.function = _bissector_f1,
	.size = 4,
};

static int _bissector_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	const double tth = x->data[3];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_f2 = {
	.function = _bissector_f2,
	.size = 4,
};

static HklMode *bissector(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&bissector_f1, &bissector_f2};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

/* constant omega */

static int _constant_omega_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double const komega = x->data[0];
	double const kappa = x->data[1];
	double omega;
	HklEngine *engine = params;
	double omega0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = omega0 - omega;

	return  GSL_SUCCESS;
}

static const HklFunction constant_omega_f1 = {
	.function = _constant_omega_f1,
	.size = 4,
};

static int _constant_omega_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	double omega;
	HklEngine *engine = params;
	double omega0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = omega0 - omega;

	return  GSL_SUCCESS;
}

static const HklFunction constant_omega_f2 = {
	.function = _constant_omega_f2,
	.size = 4,
};

static HklMode *constant_omega(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_omega_f1, &constant_omega_f2};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, constant_omega_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

/* constant chi */

static int _constant_chi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	double chi;
	HklEngine *engine = params;
	double chi0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	chi = 2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = chi0 - chi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_chi_f1 = {
	.function = _constant_chi_f1,
	.size = 4,
};

static int _constant_chi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	double chi;
	HklEngine *engine = params;
	double chi0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	chi = -2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = chi0 - chi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_chi_f2 = {
	.function = _constant_chi_f2,
	.size = 4,
};

static HklMode *constant_chi(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_chi_f1, &constant_chi_f2};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, constant_chi_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

/* constant phi */

static int _constant_phi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	const double kphi = x->data[2];
	double phi;
	HklEngine *engine = params;
	double phi0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = phi0 - phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_f1 = {
	.function = _constant_phi_f1,
	.size = 4,
};

static int _constant_phi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	const double kphi = x->data[2];
	double phi;
	HklEngine *engine = params;
	double phi0 = darray_item(engine->mode->parameters, 0)->_value;

	CHECK_NAN(x->data, x->size);

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = phi0 - phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_f2 = {
	.function = _constant_phi_f2,
	.size = 4,
};

static HklMode *constant_phi(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_phi_f1, &constant_phi_f2};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, constant_phi_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *double_diffraction(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&double_diffraction_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, double_diffraction_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *psi_constant(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, psi_constant_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_k4cv_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = bissector();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, constant_omega());
	hkl_engine_add_mode(self, constant_chi());
	hkl_engine_add_mode(self, constant_phi());
	hkl_engine_add_mode(self, double_diffraction());
	hkl_engine_add_mode(self, psi_constant());

	return self;
}

/************/
/* psi mode */
/************/

/* psi */
static HklMode *psi()
{
	static const char *axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&psi_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, psi_parameters),
	};

	return hkl_mode_psi_new(&info);
}

static HklEngine *hkl_engine_k4cv_psi_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_psi_new(engines);

	default_mode = psi();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

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
	"  + **kappa** : rotating around the :math:`\\vec{x}` direction (0, :math:`-\\cos\\alpha`, :math:`-\\sin\\alpha`)\n" \
	"  + **kphi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_kappa4C_vertical_axes[] = {"komega", "kappa", "kphi", "tth"};

static HklGeometry *hkl_geometry_new_kappa4C_vertical(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	double alpha = 50 * HKL_DEGTORAD;
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_kappa4C_vertical(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	self->geometries->multiply = hkl_geometry_list_multiply_k4c_real;
	hkl_engine_k4cv_hkl_new(self);
	hkl_engine_eulerians_new(self);
	hkl_engine_k4cv_psi_new(self);
	hkl_engine_q_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(kappa4C_vertical, "K4CV", HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION);

