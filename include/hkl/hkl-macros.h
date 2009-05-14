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
#ifndef __HKL_MACROS_H__
#define __HKL_MACROS_H__

/* Guard C code in headers, while including them from C++ */
#ifdef __cplusplus
# define HKL_BEGIN_DECLS  extern "C" {
# define HKL_END_DECLS    }
#else
# define HKL_BEGIN_DECLS
# define HKL_END_DECLS
#endif

// add the win32 portability part
#if _MSC_VER && _MSC_VER <= 1200
# include <float.h>
# define INFINITY DBL_MAX
# define M_PI     3.14159265358979323846264338328
# define M_PI_2   1.57079632679489661923132169164
#endif

// common part
#define HKL_MAJOR 2
#define HKL_MINOR 3
#define HKL_PATCH 0

#define HKL_VERSION (HKL_MAJOR * 10000 + HKL_MINOR * 100 + HKL_PATCH)

#define HKL_TRUE 1
#define HKL_FALSE 0

#define HKL_SUCCESS 0
#define HKL_FAIL -1

#define HKL_TINY 1e-7
#define HKL_EPSILON 1e-6
#define HKL_DEGTORAD (M_PI/180.)
#define HKL_RADTODEG (180./M_PI)
// tau = 2pi or 1
#define HKL_TAU (2. * M_PI)

// specific part for the eulerian -> kappa conversion
#define HKL_EULERIAN_KAPPA_SOLUTION 1

// the assert method
#ifndef NDEBUG
# include <execinfo.h>
# include <assert.h> 
# define hkl_assert(x) do{ if (!(x)) {hkl_printbt(); assert(x); } } while(0)
#else
# define hkl_assert(x)
#endif

#define alloc_nr(x) (((x)+16)*3/2)

/*
 * Realloc the buffer pointed at by variable 'x' so that it can hold
 * at least 'nr' entries; the number of entries currently allocated
 * is 'alloc', using the standard growing factor alloc_nr() macro.
 *
 * DO NOT USE any expression with side-effect for 'x' or 'alloc'.
 */
#define ALLOC_GROW(x, nr, alloc) \
	do { \
		if ((nr) > alloc) { \
			if (alloc_nr(alloc) < (nr)) \
				alloc = (nr); \
			else \
				alloc = alloc_nr(alloc); \
			x = realloc((x), alloc * sizeof(*(x))); \
		} \
	} while(0)

#ifdef __GNUC__
# define NORETURN __attribute__((__noreturn__))
#else
# define NORETURN
# ifndef __attribute__
#  define __attribute__(x)
# endif
#endif

#endif

HKL_BEGIN_DECLS

extern void die(const char *err, ...) NORETURN __attribute__((format (printf, 1, 2)));

extern void warning(const char *err, ...);

extern void hkl_printbt(void);

HKL_END_DECLS
