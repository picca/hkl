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
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
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
