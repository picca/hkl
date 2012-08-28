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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 */
#include <gsl/gsl_vector.h>
#include "hkl-pseudoaxis-auto-private.h"

typedef struct _HklPseudoAxisEngineHkl HklPseudoAxisEngineHkl;
struct _HklPseudoAxisEngineHkl {
	HklPseudoAxisEngine engine;
	HklParameter *h;
	HklParameter *k;
	HklParameter *l;
};

extern int _RUBh_minus_Q_func(const gsl_vector *x, void *params, gsl_vector *f);
extern int _double_diffraction_func(const gsl_vector *x, void *params, gsl_vector *f);
extern int _psi_constant_vertical_func(const gsl_vector *x, void *params, gsl_vector *f);

extern int RUBh_minus_Q(double const x[], void *params, double f[]);
extern int _double_diffraction(double const x[], void *params, double f[]);

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

extern int hkl_pseudo_axis_engine_mode_set_hkl_real(HklPseudoAxisEngineMode *self,
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

#define HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_HKL_DEFAULTS	\
	HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_AUTO_DEFAULTS,	\
		.get = hkl_pseudo_axis_engine_mode_get_hkl_real

static const HklPseudoAxisEngineModeOperations hkl_mode_operations = {
	HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_HKL_DEFAULTS,
};

static const HklPseudoAxisEngineModeOperations hkl_full_mode_operations = {
	HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_HKL_DEFAULTS,
	.set = hkl_pseudo_axis_engine_mode_set_hkl_real,
};

static const HklPseudoAxisEngineModeOperations psi_constant_vertical_mode_operations = {
	HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_HKL_DEFAULTS,
	.init = hkl_pseudo_axis_engine_mode_init_psi_constant_vertical_real,
};

static const HklFunction RUBh_minus_Q_func = {
	.function = _RUBh_minus_Q_func,
	.size = 3,
};

static const HklFunction double_diffraction_func = {
	.function = _double_diffraction_func,
	.size = 4,
};

static const HklFunction psi_constant_vertical_func = {
	.function = _psi_constant_vertical_func,
	.size = 4,
};
