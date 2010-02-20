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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 */
#include <hkl/hkl-pseudoaxis.h>

extern int RUBh_minus_Q_func(const gsl_vector *x, void *params, gsl_vector *f);
extern int double_diffraction_func(const gsl_vector *x, void *params, gsl_vector *f);
extern int psi_constant_vertical_func(const gsl_vector *x, void *params, gsl_vector *f);

extern int RUBh_minus_Q(double const x[], void *params, double f[]);
extern int double_diffraction(double const x[], void *params, double f[]);

/**
 * @brief Standard getter for the hkl pseudoAxis.
 * 
 * @param self 
 * @param geometry 
 * @param detector 
 * @param sample 
 * 
 * @return the status of the getter method.
 *
 * This method can be used with all geometries of diffractometers
 * in getter/setter.
 */
extern int hkl_pseudo_axis_engine_mode_get_hkl_real(HklPseudoAxisEngineMode *self,
						    HklPseudoAxisEngine *engine,
						    HklGeometry *geometry,
						    HklDetector *detector,
						    HklSample *sample,
						    HklError **error);

extern int hkl_pseudo_axis_engine_mode_init_psi_constant_vertical_real(HklPseudoAxisEngineMode *base,
								       HklPseudoAxisEngine *engine,
								       HklGeometry *geometry,
								       HklDetector *detector,
								       HklSample *sample,
								       HklError **error);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_hkl_new(void);
