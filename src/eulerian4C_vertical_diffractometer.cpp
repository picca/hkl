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
 * Authors: Picca FrÃ©dÃ©ric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "eulerian4C_vertical_diffractometer.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      Diffractometer::Diffractometer() :
          hkl::DiffractometerTemp<hkl::eulerian4C::vertical::Geometry>("Eulerian 4C Generic Soleil",
                                                                       "This diffractometer was design by Frédéric-emmanuel PICCA\n\
                                                                       * modes: bissector, delta theta, constant omega, constant chi, constant phi.\n\
                                                                       * pseudoAxes: Psi.")
      {
        // On met Ã  jour la liste des modes utilisables.
        _modes.add( new hkl::eulerian4C::vertical::mode::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", *_geom_T) );
        _modes.add( new hkl::eulerian4C::vertical::mode::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", *_geom_T) );
        _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Omega("Constant Omega", "Omega = Constante.", *_geom_T) );
        _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Chi("Constant Chi", "chi = Constante.", *_geom_T) );
        _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Phi("Constant Phi", "phi = Constante.", *_geom_T) );

        // On ajoute les pseudoAxes
        _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Q(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Psi(*_geom_T, _samples) );
      }

      Diffractometer::~Diffractometer()
      {
        // On supprime les modes.
        _modes.clear();
        // On supprime les pseudoAxes.
        _pseudoAxeEngines.clear();
      }


    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
