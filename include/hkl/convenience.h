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
#ifndef _CONVENIENCE_H
#define _CONVENIENCE_H

#include <HKLException.h>

namespace hkl
  {
  namespace convenience
    {

    /**
     * @brief return the right angle in between[_pi,pi]
     * @param angle The angle to normalize.
     * @return The angle.
     */
    double normalizeAngle(double angle);

    /*!
     * \brief Compute the atan2 function.
     * \param s The y coordinate of the point P in the xOy plan.
     * \param c The x coordinate of the point P in the xOy plan.
     * \return the angle between Ox and OP.
     */
    double atan2(double s, double c);

    /*!
     * Compute the asin function.
     * \param s The sinus of the angle.
     * \throw HKLException if \f$ /abs(s) > 1\f$.
     * \return The asinus of the angle.
     */
    double asin(double s) throw (HKLException);

  } // namespace convenience
} // namespace hkl

#endif //_CONVENIENCE_H
