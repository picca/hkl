#ifndef _CONFIG_H
#define _CONFIG_H


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

#define HKL_SUCCESS 1
#define HKL_FAIL 0

#define HKL_TINY 1e-7
#define HKL_EPSILON 1e-6
#define HKL_DEGTORAD (M_PI/180.)
#define HKL_RADTODEG (180./M_PI)
// tau = 2pi or 1
#define HKL_TAU (2. * M_PI)

// specific part for the eulerian -> kappa conversion
#define HKL_EULERIAN_KAPPA_SOLUTION 1

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

#endif

#ifdef __GNUC__
# define NORETURN __attribute__((__noreturn__))
#else
# define NORETURN
# ifndef __attribute__
#  define __attribute__(x)
# endif
#endif

extern void die(const char *err, ...) NORETURN __attribute__((format (printf, 1, 2)));
