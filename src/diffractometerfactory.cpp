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

#include "diffractometerfactory.h"
#include "diffractometer.h"

namespace hkl
  {

  DiffractometerFactory::DiffractometerFactory()
  {
  }

  DiffractometerFactory::~DiffractometerFactory()
  {
  }

  /**
   * @brief Create a new reflection.
   * @param type The hkl::DiffractometerType of the Diffractometer to create.
   * @param parameter A double use to build the Diffractometer.
   * @return The created Diffractometer.
   *
   * This parameter has no effect for an Eulerian diffractometer.
   * But correspond to the alpha angle of the Kappa Geometry for the Kappa diffractometers.
   */

  hkl::Diffractometer * DiffractometerFactory::create(hkl::DiffractometerType type, double parameter)
  {
    Diffractometer * diffractometer;

    switch (type)
      {
      case DIFFRACTOMETER_TWOC_VERTICAL :
        diffractometer = new hkl::twoC::vertical::Diffractometer;
        break;
      case DIFFRACTOMETER_EULERIAN4C_VERTICAL :
        diffractometer = new hkl::eulerian4C::vertical::Diffractometer;
        break;
      case DIFFRACTOMETER_EULERIAN6C :
        diffractometer = new hkl::eulerian6C::Diffractometer;
        break;
      case DIFFRACTOMETER_KAPPA4C_VERTICAL :
        diffractometer = new hkl::kappa4C::vertical::Diffractometer(parameter);
        break;
      case DIFFRACTOMETER_KAPPA6C :
        diffractometer = new hkl::kappa6C::Diffractometer(parameter);
        break;
      default :
        HKLEXCEPTION("Unknown diffractometer Type.", "Please use a correct type.");
      }
    return diffractometer;
  }


} // namespace hkl
