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
#ifndef _KAPPA6C_PSEUDOAXEENGINE_H
#define _KAPPA6C_PSEUDOAXEENGINE_H


#include "derived_pseudoaxeengine.h"
#include "kappa6C_geometry.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "kappa4C_vertical_pseudoaxeengine.h"
#include "eulerian6C_pseudoaxeengine.h"

namespace hkl
  {

  namespace kappa6C
    {

    namespace pseudoAxeEngine
      {

      typedef hkl::pseudoAxeEngine::DerivedWithSample<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::pseudoAxeEngine::Psi> Psi;
      typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians> Eulerians;
      typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::eulerian6C::pseudoAxeEngine::Tth> Tth;
      typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::eulerian6C::pseudoAxeEngine::Q> Q;

    } // namespace hkl::kappa6C::pseudoAxeEngine

  } // namespace hkl::kappa6C

} // namespace hkl
#endif
