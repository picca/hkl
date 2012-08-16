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
#include <hkl/hkl-pseudoaxis-e4c.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

static HklPseudoAxisEngineMode *psi(void)
{
	static const char *axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction functions[] = {psi_func};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = __func__,
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
		.functions = functions,
		.n_functions = ARRAY_SIZE(functions),
	};

	return &hkl_pseudo_axis_engine_mode_psi_new(&info)->parent;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4c_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *default_mode;

	self = hkl_pseudo_axis_engine_psi_new();

	default_mode = psi();
	hkl_pseudo_axis_engine_add_mode(self, default_mode);
	hkl_pseudo_axis_engine_select_mode(self, default_mode);

	return self;
}
