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
 * Authors: Picca FrÃ©dÃ©ric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "twoC_vertical_diffractometer.h"
#include "twoC_vertical_mode.h"
#include "twoC_vertical_pseudoaxeengine.h"

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      Diffractometer::Diffractometer() :
          hkl::DiffractometerTemp<hkl::twoC::vertical::Geometry>("2C Generic Soleil", "This diffractometer was design by Frédéric-emmanuel PICCA picca@synchrotron-soleil.fr\n\
                                                                 * modes: \"Symetric\", \"Fix incidence\"\n\
                                                                 * pseudoAxes: \"th2th\", \"q2th\", \"q\"")
      {
        // On met Ã  jour la liste des modes utilisables.
        _modes.add( new hkl::twoC::vertical::mode::Symetric("Symetric", "Omega = 2theta / 2. = theta", *_geom_T) );
        _modes.add( new hkl::twoC::vertical::mode::Fix_Incidence("Fix incidence", "2theta = 2 * theta, omega is free.", *_geom_T) );

        // On ajoute les pseudoAxes
        _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Th2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Q2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Q(*_geom_T) );
      }

      Diffractometer::~Diffractometer()
      {
        // On supprime les modes.
        _modes.clear();
        // On supprime les pseudoAxes.
        _pseudoAxeEngines.clear();
      }


    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
