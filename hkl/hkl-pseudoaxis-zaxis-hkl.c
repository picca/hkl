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
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <hkl/hkl-pseudoaxis-zaxis.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

static int reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	double mu, gamma;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	gamma = x_data[3];

	f_data[3] = mu - gamma;

	return  GSL_SUCCESS;
}

static int hkl_pseudo_axis_engine_mode_set_zaxis_hkl_real(HklPseudoAxisEngineMode *self,
							  HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample,
							  HklError **error)
{
	int res = HKL_SUCCESS;

	res &= hkl_pseudo_axis_engine_mode_set_real(self, engine,
						    geometry, detector, sample,
						    error);
	if(res == HKL_SUCCESS){
		int i;
		int len;

		/*
		 * For each solution already found we will generate another one
		 * using the Ewalds construction by rotating Q around the last sample
		 * axis of the mode until it intersect again the Ewald sphere.
		 * TODO do not work if ki is colinear with the axis.
		 */
		
		/* we will add solution to the geometries so save its length before */
		len = engine->engines->geometries->len;
		for(i=0; i<len; ++i){
			int j;
			int idx;
			HklGeometry *geom;
			HklVector ki;
			HklVector q;
			const HklAxis *axis;

			geom = engine->engines->geometries->items[i].geometry;

			/* get the Q vector kf - ki */
			hkl_detector_compute_kf(detector, geom, &q);
			hkl_source_compute_ki(&geom->source, &ki);
			hkl_vector_minus_vector(&q, &ki);

			/* get the last sample axis that can be rotated */
			/* FIX for now the sample holder is the first one */
			idx = -1;
			for(j=0; j<self->axes_names_len; ++j){
				int k;
				int tmp;

				tmp = hkl_geometry_get_axis_idx_by_name(geom, self->axes_names[j]);
				for(k=0; k<geometry->holders[0].config->len; ++k)
					if(tmp == geometry->holders[0].config->idx[k]){
						idx = idx > tmp ? idx : tmp;
						break;
					}
			}
			/* if axis not found ??? go to another geom */
			if (idx < 0)
				continue;

			axis = &geom->axes[idx];
			/*
			 * rotate the Q vector around this axis until it intersect the Ewald sphere
			 * compute the orientation of the last axis (A) with the current geometry values
			 * compute the equation of the the plan (P) with the previous axis normal an
			 * containing the Q point.
			 * Now project the center (C1) of the Ewalds sphere on this plan (P)
			 * The radius of Ewalds sphere in this plane r = distance(Q, C1)
			 * now project the origin (O) in the (P) plan -> C2
			 * the radius of the second circle r2 = distance (C2, Q)
			 * now compute the other solution (Q2) using the previous informations.
			 * Q2 is the symetric of Q along (C1C2).
			 * Maybe the right way is to just rotate Q around the axis A until it intersect
			 * the first circle.
			 */
			
			/* fit the sample part to find the position of the Detector */
		}
	}
	
	return res;
}

/*************************/
/* ZAXIS PseudoAxeEngine */
/*************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_zaxis_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* zaxis */
	mode = hkl_pseudo_axis_engine_mode_new(
		"zaxis",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_zaxis_hkl_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "delta", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* reflectivity */
	mode = hkl_pseudo_axis_engine_mode_new(
		"reflectivity",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, reflectivity,
		(size_t)0,
		(size_t)4, "mu", "omega", "delta", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
