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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-pseudoaxis.h>

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
extern int hkl_pseudo_axis_engine_mode_get_hkl_real(HklPseudoAxisEngine *self,
						       HklGeometry *geometry,
						       HklDetector const *detector,
						       HklSample const *sample);

/**
 * @brief Standard setter method for the hkl pseudoAxis
 * 
 * @param self 
 * @param geometry 
 * @param detector 
 * @param sample 
 * 
 * @return The status of the setter method.
 *
 * This method use only the hkl part of the equation to solve the 
 * pseudo axis. You can use it if there is exactly 4 unknowns.
 * exemple : All E4CV constant axis mode or E6C vertical constant axis mode.
 */
extern int hkl_pseudo_axis_engine_mode_set_hkl_real(HklPseudoAxisEngine *self,
						       HklGeometry *geometry,
						       HklDetector *detector,
						       HklSample *sample);

/**
 * @brief Standard setter method for the hkl double diffraction pseudoAxis
 *
 * @param self
 * @param geometry
 * @param detector
 * @param sample
 *
 * @return The status of the setter method.
 *
 * This method use only the hkl part of the equation to solve the
 * pseudo axis. You can use it if there is exactly 4 unknowns.
 * exemple : All E4CV constant axis mode or E6C vertical constant axis mode.
 */
extern int hkl_pseudo_axis_engine_mode_set_double_diffraction_real(HklPseudoAxisEngine *self,
								      HklGeometry *geometry,
								      HklDetector *detector,
								      HklSample *sample);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_hkl_new(void);
