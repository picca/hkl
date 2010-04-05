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

#include <gsl/gsl_sf.h>

#include <hkl/hkl-pseudoaxis-factory.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>
#include <hkl/hkl-pseudoaxis-common-q.h>
#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-k6c.h>
#include <hkl/hkl-pseudoaxis-zaxis.h>

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

static void hkl_geometry_list_multiply_k4c_real(HklGeometryList *self, size_t idx)
{
	HklGeometry *geometry;
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;

	geometry = self->items[idx]->geometry;
	komega = hkl_axis_get_value(&geometry->axes[0]);
	kappa = hkl_axis_get_value(&geometry->axes[1]);
	kphi = hkl_axis_get_value(&geometry->axes[2]);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	hkl_axis_set_value(&copy->axes[0], komegap);
	hkl_axis_set_value(&copy->axes[1], kappap);
	hkl_axis_set_value(&copy->axes[2], kphip);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}

static void hkl_geometry_list_multiply_k6c_real(HklGeometryList *self, size_t idx)
{
	HklGeometry *geometry;
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;

	geometry = self->items[idx]->geometry;
	komega = hkl_axis_get_value(&geometry->axes[1]);
	kappa = hkl_axis_get_value(&geometry->axes[2]);
	kphi = hkl_axis_get_value(&geometry->axes[3]);

	kappa_2_kappap(komega, kappa, kphi, 50 * HKL_DEGTORAD, &komegap, &kappap, &kphip);

	copy = hkl_geometry_new_copy(geometry);
	hkl_axis_set_value(&copy->axes[1], komegap);
	hkl_axis_set_value(&copy->axes[2], kappap);
	hkl_axis_set_value(&copy->axes[3], kphip);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(self, copy);
	hkl_geometry_free(copy);
}

HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_factory(const HklGeometryConfig *config)
{
	HklPseudoAxisEngineList *self = NULL;

	self = hkl_pseudo_axis_engine_list_new();

	switch(config->type){
	case HKL_GEOMETRY_TYPE_TWOC_VERTICAL:
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e4cv_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e4cv_psi_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_q_new());
		break;
	case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:
		self->geometries->multiply = hkl_geometry_list_multiply_k4c_real;
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k4cv_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_eulerians_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k4cv_psi_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_q_new());
		break;
	case HKL_GEOMETRY_TYPE_EULERIAN6C:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e6c_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e6c_psi_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_q2_new());
		break;
	case HKL_GEOMETRY_TYPE_KAPPA6C:
		self->geometries->multiply = hkl_geometry_list_multiply_k6c_real;
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k6c_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_eulerians_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k6c_psi_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_q2_new());
		break;
	case HKL_GEOMETRY_TYPE_ZAXIS:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_zaxis_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_q2_new());
		break;
	}
	return self;
}
