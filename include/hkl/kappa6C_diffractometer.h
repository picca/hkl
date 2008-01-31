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
#ifndef _KAPPA6C_DIFFRACTOMETER_H
#define _KAPPA6C_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "kappa6C_mode.h"
#include "kappa6C_pseudoaxeengine.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace kappa6C
    {

    class Diffractometer : public hkl::DiffractometerTemp<hkl::kappa6C::Geometry>
      {
      public:
        Diffractometer(double alpha);

        virtual ~Diffractometer();

      };

  } // namespace hkl::kappa6C

} // namespace hkl
#endif
