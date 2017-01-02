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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
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
#include "hkl-pseudoaxis-common-readonly-private.h"

#define OMEGA "omega"
#define CHI "chi"
#define PHI "phi"
#define TTH "tth"

/************/
/* mode hkl */
/************/

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

static HklMode *bissector(void)
{
	static const char* axes[] = {OMEGA, CHI, PHI, TTH};
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
	static const char* axes_r[] = {OMEGA, CHI, PHI, TTH};
	static const char* axes_w[] = {CHI, PHI, TTH};
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
	static const char* axes_r[] = {OMEGA, CHI, PHI, TTH};
	static const char* axes_w[] = {OMEGA, PHI, TTH};
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
	static const char* axes_r[] = {OMEGA, CHI, PHI, TTH};
	static const char* axes_w[] = {OMEGA, CHI, TTH};
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
	static const char* axes[] = {OMEGA, CHI, PHI, TTH};
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
	static const char* axes[] = {OMEGA, CHI, PHI, TTH};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, psi_constant_parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations,
				 TRUE);
}

static HklEngine *hkl_engine_e4c_hkl_new(HklEngineList *engines)
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
/* mode psi */
/************/

static HklMode *psi(void)
{
	static const char *axes[] = {OMEGA, CHI, PHI, TTH};
	static const HklFunction *functions[] = {&psi_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes, axes,
					       functions, psi_parameters),
	};

	return hkl_mode_psi_new(&info);
}

static HklEngine *hkl_engine_e4c_psi_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_psi_new(engines);

	default_mode = psi();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/*****************/
/* mode readonly */
/*****************/

REGISTER_READONLY_INCIDENCE(hkl_engine_e4c_incidence_new,
			    P99_PROTECT({OMEGA, CHI, PHI}),
			    surface_parameters_y);

REGISTER_READONLY_EMERGENCE(hkl_engine_e4c_emergence_new,
			    P99_PROTECT({OMEGA, CHI, PHI, TTH}),
			    surface_parameters_y);

/********/
/* E4CV */
/********/

#define HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **" OMEGA "** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" CHI "** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **" PHI "** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **" TTH "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_eulerian4C_vertical_axes[] = {OMEGA, CHI, PHI, TTH};

static HklGeometry *hkl_geometry_new_eulerian4C_vertical(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, -1, 0);
	hkl_holder_add_rotation_axis(h, CHI, 1, 0, 0);
	hkl_holder_add_rotation_axis(h, PHI, 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, TTH, 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_vertical(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_e4c_hkl_new(self);
	hkl_engine_e4c_psi_new(self);
	hkl_engine_q_new(self);
	hkl_engine_e4c_incidence_new(self);

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
	"  + **" OMEGA "** : rotating around the :math:`\\vec{z}` direction (0, -1, 0)\n" \
	"  + **" CHI "** : rotating around the :math:`\\vec{x}` direction (-1, 0, 0)\n" \
	"  + **" PHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **" TTH "** : rotation around the :math:`\\vec{z}` direction (0, -1, 0)\n"

static const char* hkl_geometry_soleil_mars_axes[] = {OMEGA, CHI, PHI, TTH};

static HklGeometry *hkl_geometry_new_soleil_mars(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, -1, 0);
	hkl_holder_add_rotation_axis(h, CHI, -1, 0, 0);
	hkl_holder_add_rotation_axis(h, PHI, 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, TTH, 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_soleil_mars(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_e4c_hkl_new(self);
	hkl_engine_e4c_psi_new(self);
	hkl_engine_q_new(self);
	hkl_engine_e4c_incidence_new(self);

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
	"  + **" OMEGA "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" CHI "** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **" PHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **" TTH "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

static const char* hkl_geometry_eulerian4C_horizontal_axes[] = {OMEGA, CHI, PHI, TTH};

static HklGeometry *hkl_geometry_new_eulerian4C_horizontal(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, 0, 1);
	hkl_holder_add_rotation_axis(h, CHI, 1, 0, 0);
	hkl_holder_add_rotation_axis(h, PHI, 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, TTH, 0, 0, 1);

	return self;
}

static HklEngineList *hkl_engine_list_new_eulerian4C_horizontal(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_e4c_hkl_new(self);
	hkl_engine_e4c_psi_new(self);
	hkl_engine_q_new(self);
	hkl_engine_e4c_incidence_new(self);
	hkl_engine_e4c_emergence_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(eulerian4C_horizontal, "E4CH", HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION);
