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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc

/***********************/
/* numerical functions */
/***********************/

static int _bissector_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double tth = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_func = {
	.function = _bissector_func,
	.size = 4,
};

/*********/
/* modes */
/*********/

static HklMode *bissector(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&bissector_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *constant_omega(void)
{
	static const char* axes_r[] = {"omega", "chi", "phi", "tth"};
	static const char* axes_w[] = {"chi", "phi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *constant_chi(void)
{
	static const char* axes_r[] = {"omega", "chi", "phi", "tth"};
	static const char* axes_w[] = {"omega", "phi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *constant_phi(void)
{
	static const char* axes_r[] = {"omega", "chi", "phi", "tth"};
	static const char* axes_w[] = {"omega", "chi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *double_diffraction(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&double_diffraction_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 1,},
	};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes, functions,
					       parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode *psi_constant(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "psi"},
	};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes, functions,
					       parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations,
				 TRUE);
}

/***********************/
/* pseudo axes engines */
/***********************/

HklEngine *hkl_engine_e4c_hkl_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new();

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

static HklMode *psi(void)
{
	static const char *axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&psi_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h1", .range = {.min=-1, .max=1}, ._value=1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k1", .range = {.min=-1, .max=1}, ._value=1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l1", .range = {.min=-1, .max=1}, ._value=1,},
	};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes, functions, parameters),
	};

	return hkl_mode_psi_new(&info);
}

HklEngine *hkl_engine_e4c_psi_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_psi_new();

	default_mode = psi();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/********/
/* E4CV */
/********/

#define HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_eulerian4C_vertical_axes[] = {"omega", "chi", "phi", "tth"};

static HklGeometry *hkl_geometry_new_eulerian4C_vertical(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_vertical(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian4C_vertical, "E4CV", HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION);


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

static const char* hkl_geometry_soleil_mars_axes[] = {"omega", "chi", "phi", "tth"};

static HklGeometry *hkl_geometry_new_soleil_mars(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", -1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_mars(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(soleil_mars, "SOLEIL MARS", HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION);

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

static const char* hkl_geometry_eulerian4C_horizontal_axes[] = {"omega", "chi", "phi", "tth"};

static HklGeometry *hkl_geometry_new_eulerian4C_horizontal(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, 0, 1);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_horizontal(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e4c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e4c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q_new());

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian4C_horizontal, "E4CH", HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION);


