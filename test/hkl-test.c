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
#include <stdlib.h>
#include <stdio.h>

#include <hkl/hkl-macros.h>

#include "hkl-test.h"

void hkl_tests_grow(struct hkl_tests * tests, size_t extra)
{
	if (tests->len + extra <= tests->len)
		die("you want to use way too much memory");
	if (!tests->alloc)
		tests->tests = NULL;
	ALLOC_GROW(tests->tests, tests->len + extra, tests->alloc);
}

void hkl_tests_init(struct hkl_tests *tests, size_t hint)
{
	tests->alloc = tests->len = 0;
	if (hint)
		hkl_tests_grow(tests, hint);
	else
		tests->tests = NULL;
}

void hkl_tests_release(struct hkl_tests *tests)
{
	if (tests->alloc) {
		free(tests->tests);
		hkl_tests_init(tests, 0);
	}
}

void hkl_tests_add_test(struct hkl_tests *tests, const char *name, hkl_test_method method)
{
	struct hkl_test *test;

	hkl_tests_grow(tests, 1);

	test = &tests->tests[tests->len];
	test->name = name;
	test->method = method;

	tests->len++;
}

int hkl_test_run(struct hkl_test * test)
{
	return (*(test->method))(test);
}

int hkl_tests_run(struct hkl_tests * tests)
{
	size_t i;
	int res = 0;

	for(i=0; i<tests->len; i++) {
		struct hkl_test *test = &tests->tests[i];
		if (!hkl_test_run(test)) {
			fprintf(stderr, "\n%s:%d: FAIL %s\n", test->file, test->line, test->name);
			exit(-1);
			res = -1;
			break;
		} else
			fprintf(stdout, ".");
	}
	printf("\n");
	return res;
}
