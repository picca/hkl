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

#include "eulerian6C_diffractometer.h"

namespace hkl
  {

  namespace eulerian6C
    {

    Diffractometer::Diffractometer() :
        DiffractometerTemp<hkl::eulerian6C::Geometry>("Eulerian 6C Generic Soleil", "Soleil")
    {
      // On met à jour la liste des modes utilisables.
      _modes.add( new hkl::eulerian6C::mode::Bissector("Bissector", "Bissector", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Delta_Theta("Delta Theta", "Delta Theta", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Omega("Constant Omega", "Constant Omega", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Chi("Constant Chi", "Constant Chi", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Phi("Constant Phi", "Constant Phi", *_geom_T) );

      // On met à jour les pseudo moteurs
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Tth(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Q(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Psi(*_geom_T, _samples) );
    }

    Diffractometer::~Diffractometer()
    {
      _modes.clear();
      _pseudoAxeEngines.clear();
    }


  } // namespace hkl::eulerian6C

} // namespace hkl
