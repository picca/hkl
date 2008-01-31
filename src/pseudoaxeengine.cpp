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

#include "pseudoaxeengine.h"
#include "pseudoaxe.h"

namespace hkl
  {

  PseudoAxeEngine::PseudoAxeEngine() :
      HKLObject("engine", "engine")
  {
  }

  /**
   * @brief The default destructor.
   */
  PseudoAxeEngine::~PseudoAxeEngine()
  {
  }

  /**
   * @brief Set the read part of a PseudoAxe without calling the set methode of the engine
   * @param pseudoAxe the hkl::PseudoAxe to set
   * @param min the minimum value to set
   * @param current the current value to set
   * @param consign the consign value to set
   * @param max The maximum value to set
   */
  void PseudoAxeEngine::set_pseudoAxe(hkl::PseudoAxe * pseudoAxe, double min, double current, double consign, double max)
  {
    pseudoAxe->_min.set_value(min);
    pseudoAxe->_current.set_value(current);
    pseudoAxe->_consign.set_value(consign);
    pseudoAxe->_max.set_value(max);
  }


} // namespace hkl
