#ifndef _PORTABILITY_H
# define _PORTABILITY_H

// common part
# define HKL_MAJOR 2
# define HKL_MINOR 3
# define HKL_PATCH 0

# define HKL_VERSION (HKL_MAJOR * 10000 + HKL_MINOR * 100 + HKL_PATCH)

// now add the win32 portability part
# if _MSC_VER && _MSC_VER <= 1200
#   include <float.h>
#   define INFINITY DBL_MAX
#   define M_PI     3.14159265358979323846264338328
#   define M_PI_2   1.57079632679489661923132169164
# endif

#endif
