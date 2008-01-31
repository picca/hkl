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

#include "constant.h"

namespace hkl
  {

  double constant::math::epsilon = 1e-6;

  double constant::math::tiny = 1e-7;

  int constant::math::precision = 7;

  double constant::math::pi = 3.14159265358979323846;

  double constant::math::degToRad = 0.01745329251994330;

  double constant::math::radToDeg = 57.2957795130823208;

  double constant::physic::tau = 2 * 3.14159265358979323846;


} // namespace hkl
