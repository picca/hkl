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
#ifndef _EULERIAN4C_VERTICAL_DIFFRACTOMETER_H
#define _EULERIAN4C_VERTICAL_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "eulerian4C_vertical_mode.h"
#include "eulerian4C_vertical_geometry.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      class Diffractometer : public hkl::DiffractometerTemp<hkl::eulerian4C::vertical::Geometry>
        {
        public:
          Diffractometer();

          virtual ~Diffractometer();

        };

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
#endif
