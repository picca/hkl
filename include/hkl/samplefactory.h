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
#ifndef _SAMPLEFACTORY_H
#define _SAMPLEFACTORY_H


#include <vector>
#include "sample.h"
#include <string>
#include "HKLException.h"

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class Sample;
}

namespace hkl
  {

  class SampleFactory
    {
    protected:
      hkl::Geometry & _geometry;


    public:
      /**
       * @brief The default constructor.
       * @param geometry the Geometry use to fill the Reflection._geometry.
       */

      SampleFactory(hkl::Geometry & geometry);

      std::vector<SampleType> types() const;

      /**
       * @brief Create a new reflection.
       * @return The created Reflection.
       */

      hkl::Sample * create(const std::string & name, hkl::SampleType type) const throw(hkl::HKLException);

    };

} // namespace hkl
#endif
