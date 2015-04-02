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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_MACROS_PRIVATE_H__
#define __HKL_MACROS_PRIVATE_H__

#include <assert.h>                     // for assert
#include "hkl.h"                        // for G_BEGIN_DECLS, etc

/* specific part for the eulerian -> kappa conversion */
#define HKL_EULERIAN_KAPPA_SOLUTION 1

/* the assert method */
#if !defined(NDEBUG) && !_MSC_VER
# include <execinfo.h>
# include <assert.h>
# define hkl_assert(x) do{ if (!(x)) {hkl_printbt(); assert(x); } } while(0)
#else
# define hkl_assert(x)
#endif

#define hkl_error(expr) do{				\
		if(!(expr)){				\
			g_error(__STRING(expr));	\
		}					\
	} while(0)

/* use for the printf format methods took from glib */
#define G_GNUC_PRINTF( format_idx, arg_idx )				\
	__attribute__((__format__ (__printf__, format_idx, arg_idx)))

/* use for the hkl_list */
#define alloc_nr(x) (((x)+16)*3/2)

/*
 * Realloc the buffer pointed at by variable 'x' so that it can hold
 * at least 'nr' entries; the number of entries currently allocated
 * is 'alloc', using the standard growing factor alloc_nr() macro.
 *
 * DO NOT USE any expression with side-effect for 'x' or 'alloc'.
 */
#define ALLOC_GROW(x, nr, alloc)				\
	do {							\
		if ((nr) > alloc) {				\
			if (alloc_nr(alloc) < (nr))		\
				alloc = (nr);			\
			else					\
				alloc = alloc_nr(alloc);	\
			x = realloc((x), alloc * sizeof(*(x))); \
		}						\
	} while(0)

#ifndef NORETURN
# ifdef __GNUC__
#  define NORETURN __attribute__((__noreturn__))
# else
#  define NORETURN
#  ifndef __attribute__
#   define __attribute__(x)
#  endif
# endif
#endif

#define DARRAY(_items) {.item=_items, .size=ARRAY_SIZE(_items), .alloc=ARRAY_SIZE(_items)}

G_BEGIN_DECLS

extern void hkl_printbt(void);

void *_hkl_malloc(int size, const char *error);

G_END_DECLS

/* malloc method */
#define HKL_MALLOC(type) (type *)_hkl_malloc(sizeof(type), "Can not allocate memory for a " #type)

#endif
