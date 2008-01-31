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
#ifndef _KAPPA4C_VERTICAL_MODE_H
#define _KAPPA4C_VERTICAL_MODE_H


#include "derived_mode.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian4C_vertical_mode.h"

namespace hkl
  {

  namespace kappa4C
    {

    namespace vertical
      {

      namespace mode
        {

        typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Bissector> Bissector;
        typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Delta_Theta> Delta_Theta;
        typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Omega> Constant_Omega;
        typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Chi> Constant_Chi;
        typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Phi> Constant_Phi;

      } // namespace hkl::kappa4C::vertical::mode

    } // namespace hkl::kappa4C::vertical

  } // namespace hkl::kappa4C

} // namespace hkl
#endif
