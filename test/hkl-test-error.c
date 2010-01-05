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
#include <hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME error

HKL_TEST_SUITE_FUNC(new)
{
	HklError *error;

	error = hkl_error_new("salut %s", "fred");
	HKL_ASSERT_STRING_EQUAL(error->message, "salut fred");
	hkl_error_free(error);

	error = hkl_error_new_literal("salut fred");
	HKL_ASSERT_STRING_EQUAL(error->message, "salut fred");
	hkl_error_free(error);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(copy)
{
	HklError *error;
	HklError *copy;

	error = hkl_error_new ("salut %s", "fred");
	copy = hkl_error_new_copy (error);

	HKL_ASSERT_STRING_EQUAL (copy->message, "salut fred");

	hkl_error_free (error);
	hkl_error_free (copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklError *error = NULL;

	hkl_error_set (&error, "salut %s", "bob");
	HKL_ASSERT_STRING_EQUAL (error->message, "salut bob");
	hkl_error_clear (&error);

	hkl_error_set_literal (&error, "salut fred");
	HKL_ASSERT_STRING_EQUAL (error->message, "salut fred");

	hkl_error_free (error);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(propagate)
{
	HklError *dest = NULL;
	HklError *src;

	src = hkl_error_new ("salut %s", "fred");
	hkl_error_propagate (NULL, src);
	/* free src so no memory leak here*/

	src = hkl_error_new ("salut %s", "fred");
	hkl_error_propagate (&dest, src);
	HKL_ASSERT_STRING_EQUAL (dest->message, "salut fred");
	/* transfer the error into dest so no need to release src anymore */
	hkl_error_free (dest);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(clear)
{
	HklError *error;

	error = hkl_error_new ("salut %s", "fred");
	hkl_error_clear (&error);

	HKL_ASSERT_POINTER_EQUAL (NULL, error);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(prefix)
{
	HklError *error;
	HklError *dest = NULL;

	error = hkl_error_new ("%s", "fred");

	hkl_error_prefix (&error, "%s", "salut ");
	HKL_ASSERT_STRING_EQUAL (error->message, "salut fred");

	hkl_error_propagate_prefixed (&dest, error, "%s", "oh ");
	HKL_ASSERT_STRING_EQUAL (dest->message, "oh salut fred");

	hkl_error_free (dest);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( copy );
HKL_TEST( set );
HKL_TEST( propagate );
HKL_TEST( clear );
HKL_TEST( prefix );

HKL_TEST_SUITE_END
