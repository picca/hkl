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

#include "reflectionfactory.h"
#include "geometry.h"

#include "reflection_monocrystal.h"
namespace hkl
  {

  /**
   * @brief The default constructor.
   * @param geometry the Geometry use to fill the Reflection._geometry.
   * @param type the type of the Reflection.
   */

  ReflectionFactory::ReflectionFactory(hkl::Geometry & geometry, hkl::ReflectionType type)  :
      _geometry(geometry),
      _type(type)
  {
  }

  /**
   * @brief Create a new reflection.
   * @return The created Reflection.
   */

  hkl::Reflection * ReflectionFactory::create() const throw(hkl::HKLException)
  {
    Reflection * reflection;

    switch (_type)
      {
      case REFLECTION_MONOCRYSTAL :
        reflection = new reflection::MonoCrystal(_geometry, svector(), true);
        break;
      default :
        HKLEXCEPTION("Unknown reflection Type.", "Please use a correct type.");
      }
    return reflection;
  }


} // namespace hkl
