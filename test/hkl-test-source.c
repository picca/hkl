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
#include <math.h>

#include <hkl/hkl-source.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME source

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklSource s, c;

	hkl_source_init(&s, 1.54, 1, 0, 0);
	c = s;

	HKL_ASSERT_DOUBLES_EQUAL(c.wave_length, s.wave_length, HKL_EPSILON);
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_vector_cmp(&c.direction, &s.direction));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(init)
{
	HklSource s;

	hkl_source_init(&s, 1, 1, 0, 0);

	HKL_ASSERT_DOUBLES_EQUAL(1., s.wave_length, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., s.direction.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., s.direction.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., s.direction.data[2], HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklSource ref, s1, s2;

	hkl_source_init(&ref, 1.54, 1, 0, 0);
	hkl_source_init(&s1, 1.54, 1, 0, 0);
	hkl_source_init(&s2, 1, 1, 0, 0);

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&ref, &s1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_source_cmp(&ref, &s2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_ki)
{
	HklSource s;
	HklVector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
	HklVector ki;

	hkl_source_init(&s, 1.54, 1, 0, 0);

	hkl_source_compute_ki(&s, &ki);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&ki_ref, &ki));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new_copy );
HKL_TEST( init );
HKL_TEST( cmp );
HKL_TEST( compute_ki );

HKL_TEST_SUITE_END
