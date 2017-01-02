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
#include "hkl-pseudoaxis-common-hkl-private.h"

#define MU "mu"
#define OMEGA "omega"
#define CHI "chi"
#define PHI "phi"
#define DELTA "delta"
#define GAMMA "gamma"

/********/
/* mode */
/********/

static HklMode *zaxis_alpha_fixed()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {OMEGA, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + alpha-fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *zaxis_beta_fixed()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {MU, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + beta-fixed", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

/* zaxis + alpha=beta */

static int _reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = mu - gamma;

	return  GSL_SUCCESS;
}

static const HklFunction reflectivity = {
	.function = _reflectivity,
	.size = 4,
};


static HklMode *zaxis_alpha_eq_beta()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {MU, OMEGA, DELTA, GAMMA};
	static const HklFunction *functions[] = {&reflectivity};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("zaxis + alpha=beta", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

/* 4-circles bissecting horizontal */

static int _bissector_horizontal(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double delta = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = delta - 2 * fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_horizontal = {
	.function = _bissector_horizontal,
	.size = 4,
};

static HklMode *fourc_bissector_horizontal()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {OMEGA, CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&bissector_horizontal};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles bissecting horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *fourc_constant_omega_horizontal()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {CHI, PHI, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant omega horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *fourc_constant_chi_horizontal()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {OMEGA, PHI, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant chi horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *fourc_constant_phi_horizontal()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {OMEGA, CHI, DELTA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("4-circles constant phi horizontal", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_mu()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {MU, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector mu", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_omega()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {OMEGA, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector omega", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_chi()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {CHI, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector chi", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

static HklMode *lifting_detector_phi()
{
	static const char *axes_r[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};
	static const char *axes_w[] = {PHI, DELTA, GAMMA};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO("lifting detector phi", axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

/**********************/
/* pseudo axis engine */
/**********************/

static HklEngine *hkl_engine_petra3_p09_eh2_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = zaxis_alpha_fixed();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	hkl_engine_add_mode(self, zaxis_beta_fixed());
	hkl_engine_add_mode(self, zaxis_alpha_eq_beta());
	hkl_engine_add_mode(self, fourc_bissector_horizontal());
	hkl_engine_add_mode(self, fourc_constant_omega_horizontal());
	hkl_engine_add_mode(self, fourc_constant_chi_horizontal());
	hkl_engine_add_mode(self, fourc_constant_phi_horizontal());
	hkl_engine_add_mode(self, lifting_detector_mu());
	hkl_engine_add_mode(self, lifting_detector_omega());
	hkl_engine_add_mode(self, lifting_detector_chi());
	hkl_engine_add_mode(self, lifting_detector_phi());

	return self;
}

/******************/
/* PETRA3 P09 EH2 */
/******************/

#define HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **" MU "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" OMEGA "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" CHI "** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **" PHI "** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **" MU "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **" DELTA "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" GAMMA "** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

static const char* hkl_geometry_petra3_p09_eh2_axes[] = {MU, OMEGA, CHI, PHI, DELTA, GAMMA};

static HklGeometry *hkl_geometry_new_petra3_p09_eh2(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory);
	HklHolder *h;

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, MU, 0, -1, 0);
	hkl_holder_add_rotation_axis(h, OMEGA, 0, 0, 1);
	hkl_holder_add_rotation_axis(h, CHI, 1, 0, 0);
	hkl_holder_add_rotation_axis(h, PHI, 0, 0, 1);

	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, MU, 0, -1, 0);
	hkl_holder_add_rotation_axis(h, DELTA, 0, 0, 1);
	hkl_holder_add_rotation_axis(h, GAMMA, 0, -1, 0);

	return self;
}

static HklEngineList *hkl_engine_list_new_petra3_p09_eh2(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_petra3_p09_eh2_hkl_new(self);

	return self;
}

REGISTER_DIFFRACTOMETER(petra3_p09_eh2, "PETRA3 P09 EH2", HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION);
