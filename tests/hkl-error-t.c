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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>

#include "hkl-error-private.h"

static void new(void)
{
	HklError *error;

	error = hkl_error_new("salut %s", "fred");
	is_string("salut fred", error->message, __func__);
	hkl_error_free(error);

	error = hkl_error_new_literal("salut fred");
	is_string("salut fred", error->message, __func__);
	hkl_error_free(error);
}

static void copy(void)
{
	HklError *error;
	HklError *copy;

	error = hkl_error_new ("salut %s", "fred");
	copy = hkl_error_new_copy (error);

	is_string ("salut fred", copy->message, __func__);

	hkl_error_free (error);
	hkl_error_free (copy);
}

static void set(void)
{
	HklError *error = NULL;

	hkl_error_set (&error, "salut %s", "bob");
	is_string ("salut bob", error->message, __func__);
	hkl_error_clear (&error);

	hkl_error_set_literal (&error, "salut fred");
	is_string ("salut fred", error->message, __func__);

	hkl_error_free (error);
}

static void propagate(void)
{
	HklError *dest = NULL;
	HklError *src;

	src = hkl_error_new ("salut %s", "fred");
	hkl_error_propagate (NULL, src);
	/* free src so no memory leak here*/

	src = hkl_error_new ("salut %s", "fred");
	hkl_error_propagate (&dest, src);
	is_string ("salut fred", dest->message, __func__);
	/* transfer the error into dest so no need to release src anymore */
	hkl_error_free (dest);
}

static void clear(void)
{
	HklError *error;

	error = hkl_error_new ("salut %s", "fred");
	hkl_error_clear (&error);

	ok(NULL == error, __func__);
}

static void prefix(void)
{
	HklError *error;
	HklError *dest = NULL;

	error = hkl_error_new ("%s", "fred");

	hkl_error_prefix (&error, "%s", "salut ");
	is_string ("salut fred", error->message, __func__);

	hkl_error_propagate_prefixed (&dest, error, "%s", "oh ");
	is_string ("oh salut fred", dest->message, __func__);

	hkl_error_free (dest);
}

int main(int argc, char** argv)
{
	plan(9);

	new();
	copy();
	set();
	propagate();
	clear();
	prefix();

	return 0;
}
