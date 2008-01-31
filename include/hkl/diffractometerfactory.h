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
#ifndef _DIFFRACTOMETERFACTORY_H
#define _DIFFRACTOMETERFACTORY_H


#include "twoC_vertical_diffractometer.h"
#include "eulerian4C_vertical_diffractometer.h"
#include "kappa4C_vertical_diffractometer.h"
#include "eulerian6C_diffractometer.h"
#include "kappa6C_diffractometer.h"

namespace hkl
  {
  class Diffractometer;
}

namespace hkl
  {

  enum DiffractometerType
  {
    DIFFRACTOMETER_TWOC_VERTICAL,
    DIFFRACTOMETER_EULERIAN4C_VERTICAL,
    DIFFRACTOMETER_KAPPA4C_VERTICAL,
    DIFFRACTOMETER_EULERIAN6C,
    DIFFRACTOMETER_KAPPA6C
  };
  class DiffractometerFactory
    {
    public:
      DiffractometerFactory();

      virtual ~DiffractometerFactory();

      /**
       * @brief Create a new reflection.
       * @param type The hkl::DiffractometerType of the Diffractometer to create.
       * @param parameter A double use to build the Diffractometer.
       * @return The created Diffractometer.
       *
       * This parameter has no effect for an Eulerian diffractometer.
       * But correspond to the alpha angle of the Kappa Geometry for the Kappa diffractometers.
       */

      hkl::Diffractometer * create(hkl::DiffractometerType type, double parameter);

    };

} // namespace hkl
#endif
