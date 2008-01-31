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

#include "reflection_monocrystal.h"
#include "geometry.h"
#include "svector.h"

namespace hkl
  {

  namespace reflection
    {

    /**
     * @brief The default constructor.
     *
     * @param geometry The geometry use to initialize the geometry store in the reflections.
     * @param hkl the scattering vector of the reflection.
     * @param flag the falg of the reflection (related to the affinement).
     * @throw HKLException if the geometry is not valid.
     */

    MonoCrystal::MonoCrystal(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) throw(hkl::HKLException) :
        Reflection( geometry, hkl, flag )
    {
      // do not forgot to update _hkl_phi
      _hkl_phi = _geometry.get_sample_rotation_matrix().transpose() * _geometry.get_Q();
    }

    MonoCrystal::~MonoCrystal()
    {
    }

    /**
     * @brief The copy constructor
     * @param reflection  The Reflection to copy.
     */

    MonoCrystal::MonoCrystal(const hkl::reflection::MonoCrystal & source) :
        Reflection(source)
    {
    }

    /**
     * @brief clone the reflection.
     * @return a cloned reflection.
     */

    hkl::Reflection * MonoCrystal::clone() const
      {
        return new MonoCrystal(*this);
      }


  } // namespace hkl::reflection

} // namespace hkl
