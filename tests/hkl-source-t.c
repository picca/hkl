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

#include <hkl/hkl-source.h>

static void new_copy(void)
{
	HklSource s, c;

	hkl_source_init(&s, 1.54, 1, 0, 0);
	c = s;

	is_double(c.wave_length, s.wave_length, HKL_EPSILON, __func__);
	ok(HKL_FALSE == hkl_vector_cmp(&c.direction, &s.direction), __func__);
}

static void init(void)
{
	HklSource s;

	hkl_source_init(&s, 1, 1, 0, 0);

	is_double(1., s.wave_length, HKL_EPSILON, __func__);
	is_double(1., s.direction.data[0], HKL_EPSILON, __func__);
	is_double(0., s.direction.data[1], HKL_EPSILON, __func__);
	is_double(0., s.direction.data[2], HKL_EPSILON, __func__);
}

static void cmp(void)
{
	HklSource ref, s1, s2;

	hkl_source_init(&ref, 1.54, 1, 0, 0);
	hkl_source_init(&s1, 1.54, 1, 0, 0);
	hkl_source_init(&s2, 1, 1, 0, 0);

	ok(HKL_TRUE == hkl_source_cmp(&ref, &s1), __func__);
	ok(HKL_FALSE == hkl_source_cmp(&ref, &s2), __func__);
}

static void compute_ki(void)
{
	HklSource s;
	HklVector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
	HklVector ki;

	hkl_source_init(&s, 1.54, 1, 0, 0);

	hkl_source_compute_ki(&s, &ki);
	ok(0 == hkl_vector_cmp(&ki_ref, &ki), __func__);
}

static void get_wavelength(void)
{
	HklSource s;

	hkl_source_init(&s, 1, 1, 0, 0);

	is_double(1., hkl_source_get_wavelength(&s), HKL_EPSILON, __func__);
}

int main(int argc, char** argv)
{
	plan(10);

	new_copy();
	init();
	cmp();
	compute_ki();
	get_wavelength();

	return 0;
}
