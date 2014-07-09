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
 * Copyright (C) 2013-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-pseudoaxis-auto-private.h"  // for hkl_mode_auto_new, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_engine_hkl_new, etc
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_add_mode
#include "hkl.h"                        // for HklEngine, HklMode, etc

/**************************/
/* TURRET PseudoAxeEngine */
/**************************/

static HklMode* lifting_detector_thetah()
{
	static const char *axes_r[] = {"thetah", "alphay", "alphax", "delta", "gamma"};
	static const char* axes_w[] = {"thetah", "delta", "gamma"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations,
				 TRUE);
}

HklEngine *hkl_engine_soleil_sirius_turret_hkl_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new();

	default_mode = lifting_detector_thetah();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}
