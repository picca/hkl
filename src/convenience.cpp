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
#include "portability.h"

#include <cmath>

#include "constant.h"
#include "convenience.h"

namespace hkl
  {
  namespace convenience
    {

    double
    normalizeAngle(double angle)
    {
      double res = ::atan2(::sin(angle), ::cos(angle));
      if (fabs(res - constant::math::pi) < constant::math::epsilon
          && angle < 0)
        res = -res;

      return res;
    }

    double
    atan2(double s, double c)
    {
      double angle;

      if (fabs(s) < constant::math::epsilon) s = 0.;
      if (fabs(c) < constant::math::epsilon) c = 0.;
      angle = ::atan2(s, c);
      if (fabs(angle) < constant::math::epsilon) angle = 0.;
      return angle;
    }

    double
    asin(double s) throw (HKLException)
    {
      double angle;
      if (fabs(s) - 1. > constant::math::epsilon)
        HKLEXCEPTION("sinus bigger than 1.", "");
      else
        angle = ::asin(s);

      if (fabs(angle) < constant::math::epsilon) angle = 0.;

      return angle;
    }

  } // namespace convenience
} // namespace hkl
