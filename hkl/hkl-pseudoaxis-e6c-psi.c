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
 */
#include <ccan/array_size/array_size.h>

#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-common-psi-private.h"

/********/
/* mode */
/********/

static HklMode* psi_vertical()
{
	static const char *axes[] = {"omega", "chi", "phi", "delta"};
	static const HklFunction *functions[] = {&psi_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h1", .range = {.min=-1, .max=1}, ._value=1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k1", .range = {.min=-1, .max=1}, ._value=1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l1", .range = {.min=-1, .max=1}, ._value=1,},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_psi_new(&info);
}

/**********************/
/* pseudo axis engine */
/**********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklMode *default_mode;

	self = hkl_pseudo_axis_engine_psi_new();

	default_mode = psi_vertical();
	hkl_pseudo_axis_engine_add_mode(self, default_mode);
	hkl_pseudo_axis_engine_select_mode(self, default_mode);

	return self;
}
