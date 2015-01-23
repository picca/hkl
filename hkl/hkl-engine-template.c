/* COPY THIS FILE INTO hkl-engine-xxx.c and fill the XXX */
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
 * Copyright (C) 2014      XXX copyright owner XXX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          XXXX <xxx@xxx>
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-q-private.h"  // for hkl_engine_q2_new, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-psi-private.h"  // for hkl_engine_psi_new, etc

/**************/
/* Axes names */
/**************/

#define MU "mu"
#define OMEGA "omega"
#define CHI "chi"
#define PHI "phi"
#define GAMMA "gamma"
#define DELTA "delta"

/************/
/* Geometry */
/************/

#define HKL_GEOMETRY_EULERIAN6C_DESCRIPTION				\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **"MU"** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **"OMEGA"** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **"CHI"** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **"PHI"** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **"GAMMA"** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **"DELTA"** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_eulerian6C_axes[] = {MU, OMEGA, CHI, PHI, GAMMA, DELTA};

static HklGeometry *hkl_geometry_new_eulerian6C(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, MU, 0, 0, 1);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, -1, 0);
	hkl_holder_add_rotation_axis(h, CHI, 1, 0, 0);
	hkl_holder_add_rotation_axis(h, PHI, 0, -1, 0);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, GAMMA, 0, 0, 1);
	hkl_holder_add_rotation_axis(h, DELTA, 0, -1, 0);

	return self;
}

/*********/
/* Modes */
/*********/

/* exemple of a lowlevel gsl function use to compute an hkl bissector
 * vertical mode for an E6C diffractometer */
static int _bissector_vertical_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double tth = x->data[3];

	/* this method check that all the x values are valid. Sometime
	 * the computation produce NAN values. In that case
	 * computation is skipped */
	CHECK_NAN(x->data, x->size);

	/* do the hkl computation which fill the f[0..2] values */
	RUBh_minus_Q(x->data, params, f->data);

	/* here a mode specific equation requiered due to the number
	 * of axes to fit (4 in this case) */
	f->data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

/* Declare the number of axes expected by the gsl low level
 * function. So during the HklMode configuration there is a runtime
 * check which ensure that the right number of axes are given to the
 * HklMode. */
static const HklFunction bissector_vertical_func = {
	.function = _bissector_vertical_func,
	.size = 4,
};

/* exemple of a mode with 4 axes. In that case you need the previously
 * defined function */
static HklMode *bissector_vertical(void)
{
	/* axes_r is the axes list requiered to compute the pseudo axes values */
	static const char* axes_r[] = {MU, OMEGA, CHI, PHI, GAMMA, DELTA};

	/* axes_w is the axes list use when you write the pseudo axes
	 * values. You move only thoses axes when you use this
	 * mode. */
	static const char* axes_w[] = {OMEGA, CHI, PHI, DELTA};

	/* here a list of functions use to solve the mode */
	static const HklFunction *functions[] = {&bissector_vertical_func};

	/* here just the description of the mode: name, axes_r, axes_w, functions */
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	/* instantiate a new mode */
	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

/* here an exemple of a three axes hkl mode, a convenience function is
 * provided to do the computation (RUBh_minus_Q_func). This funtion
 * takes only three axes. So writing a generic hkl mode with only
 * three axes is really simple */
static HklMode *constant_omega_vertical(void)
{
	static const char* axes_r[] = {MU, OMEGA, CHI, PHI, GAMMA, DELTA};
	static const char* axes_w[] = {CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

static HklMode* psi_vertical()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, GAMMA, DELTA};
	static const char *axes_w[] = {OMEGA, CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&psi_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO_WITH_PARAMS(__func__, axes_r, axes_w,
					       functions, psi_parameters),
	};

	return hkl_mode_psi_new(&info);
}

/***********/
/* Engines */
/***********/

static HklEngine *hkl_engine_e6c_hkl_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new();

	default_mode = bissector_vertical();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, constant_omega_vertical());

	return self;
}

static HklEngine *hkl_engine_e6c_psi_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_psi_new();

	default_mode = psi_vertical();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/***************/
/* Engine list */
/***************/

static HklEngineList *hkl_engine_list_new_eulerian6C(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_list_add(self, hkl_engine_e6c_hkl_new());
	hkl_engine_list_add(self, hkl_engine_e6c_psi_new());
	hkl_engine_list_add(self, hkl_engine_q2_new());
	hkl_engine_list_add(self, hkl_engine_qper_qpar_new());

	return self;
}

/* Register the diffractometer into the factory */
REGISTER_DIFFRACTOMETER(eulerian6C, "E6C", HKL_GEOMETRY_EULERIAN6C_DESCRIPTION);

