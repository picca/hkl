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
