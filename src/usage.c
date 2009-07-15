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
/*
 * Copyright (C) Linus Torvalds, 2005-2006.
 * Copyright (C) Synchrotron Soleil 2007-2008.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <hkl/hkl-macros.h>

static void report(const char *prefix, const char *err, va_list params)
{
	fputs(prefix, stderr);
	vfprintf(stderr, err, params);
	fputs("\n", stderr);
}

static NORETURN void die_builtin(const char *err, va_list params)
{
	report("fatal: ", err, params);
	exit(128);
}
static void warning_builtin(const char *err, va_list params)
{
	report("warning: ", err, params);
}


/* If we are in a dlopen()ed .so write to a global variable would segfault
 * (ugh), so keep things static. */
static void (*die_routine)(const char *err, va_list params) NORETURN = die_builtin;
static void (*warning_routine)(const char *err, va_list params) = warning_builtin;

void set_die_routine(void (*routine)(const char *err, va_list params) NORETURN)
{
	die_routine = routine;
}

void set_warning_routine(void (*routine)(const char *err, va_list params))
{
	warning_routine = routine;
}

void die(const char *err, ...)
{
	va_list params;

	va_start(params, err);
	die_routine(err, params);
	va_end(params);
}

void warning(const char *err, ...)
{
	va_list params;

	va_start(params, err);
	warning_routine(err, params);
	va_end(params);
}

void hkl_printbt(void)
{
	void *array[20];
	int size;
	char **strings;
	int i;

	size = backtrace(array, 20);
	strings = backtrace_symbols(array, size);

	printf("Got a backtrace:\n");
	for(i=0; i<size; ++i)
		printf("#%i %s\n", i, strings[i]);

	free(strings);
}
