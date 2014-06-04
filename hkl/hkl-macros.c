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
#include <execinfo.h>                   // for backtrace, etc
#include <stdio.h>                      // for fprintf, printf, stderr
#include <stdlib.h>                     // for calloc, exit, free

#ifndef _MSC_VER
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
		fprintf(stderr, "#%i %s\n", i, strings[i]);

	free(strings);
}
#else
int vasprintf(char **strp, const char *fmt, va_list ap)
{
	int len;
	char *buffer;

	len = vsnprintf(*strp, 0, fmt, ap);
	buffer = malloc(len);
	vsnprintf(buffer, len-1, fmt, ap);
	*strp = buffer;

	return len;
}
#endif

void *_hkl_malloc(int size, const char *error)
{
	void *tmp;

	tmp = calloc(1, size);
	if(!tmp){
		fprintf(stderr, "%s", error);
		exit(128);
	}

	return tmp;
}
