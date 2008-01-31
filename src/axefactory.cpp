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

#include "axefactory.h"
#include "axe.h"

namespace hkl
  {

  /**
   * @brief Create a new reflection.
   * @param type The type of the Axe to create.
   * @param name The name of the Axe to add.
   * @return The created Reflection.
   *
   * This method is only use in the holderList fromStream Method.
   */
  hkl::Axe * AxeFactory::create(hkl::AxeType type, const std::string & name) throw(hkl::HKLException)

  {
    hkl::Axe * axe = NULL;

    switch (type)
      {
      case AXE_ROTATION :
        axe = new hkl::axe::Rotation(name, "rotation", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(1, 1, 1));
        break;
      default :
        HKLEXCEPTION("Unknown axe Type.", "Please use a correct type.");
      }
    return axe;
  }


} // namespace hkl
