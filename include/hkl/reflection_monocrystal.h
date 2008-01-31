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
#ifndef _REFLECTION_MONOCRYSTAL_H
#define _REFLECTION_MONOCRYSTAL_H


#include "reflection.h"
#include "HKLException.h"

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class svector;
}

namespace hkl
  {

  namespace reflection
    {

    class MonoCrystal : public hkl::Reflection
      {
      public:
        /**
         * @brief The default constructor.
         *
         * @param geometry The geometry use to initialize the geometry store in the reflections.
         * @param hkl the scattering vector of the reflection.
         * @param flag the falg of the reflection (related to the affinement).
         * @throw HKLException if the geometry is not valid.
         */

        MonoCrystal(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) throw(hkl::HKLException);

        virtual ~MonoCrystal();

        /**
         * @brief The copy constructor
         * @param reflection  The Reflection to copy.
         */

        MonoCrystal(const MonoCrystal & source);

        /**
         * @brief clone the reflection.
         * @return a cloned reflection.
         */

        hkl::Reflection * clone() const;

      };

  } // namespace hkl::reflection

} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class reflection
 * @param flux The flux to print into
 * @param reflection The Reflection to print.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::reflection::MonoCrystal const & reflection)
{
  return reflection.printToStream(flux);
}
#endif
