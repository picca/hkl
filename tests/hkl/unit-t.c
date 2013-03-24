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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>
#include <tap/basic.h>
#include <tap/float.h>

static void compatible(void)
{
	int res = HKL_TRUE;

	const HklUnit *unit;
	const HklUnit *punit;

	res &= hkl_unit_compatible(&hkl_unit_angle_deg, &hkl_unit_angle_rad);
	res &= !hkl_unit_compatible(&hkl_unit_angle_deg, &hkl_unit_length_nm);
	res &= !hkl_unit_compatible(&hkl_unit_angle_rad, &hkl_unit_length_nm);
	res &= hkl_unit_compatible(NULL, NULL);

	ok(res == HKL_TRUE, __func__);
}

static void factor(void)
{
	is_double(HKL_DEGTORAD, hkl_unit_factor(&hkl_unit_angle_deg, &hkl_unit_angle_rad), HKL_EPSILON, __func__);
}

int main(int argc, char** argv)
{
	plan(2);

	compatible();
	factor();

	return 0;
}
