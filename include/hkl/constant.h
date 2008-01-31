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
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _CONSTANT_H
#define _CONSTANT_H

#include <cmath>
#include <iostream>

#include "HKLException.h"

namespace hkl
  {

  class constant
    {
    public:
      class math
        {
        public:
          static double epsilon;

          static double tiny;

          static int precision;

          static double pi;

          static double degToRad;

          static double radToDeg;

        };
#define SOLUTION 1

      class physic
        {
        public:
          static double tau;

        };

    };

} // namespace hkl
#endif
