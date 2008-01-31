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

#include "samplefactory.h"
#include "geometry.h"

#include "sample_monocrystal.h"
namespace hkl
  {

  /**
   * @brief The default constructor.
   * @param geometry the Geometry use to fill the Reflection._geometry.
   */

  SampleFactory::SampleFactory(hkl::Geometry & geometry) :
      _geometry(geometry)
  {
  }

  std::vector<SampleType> SampleFactory::types() const
    {
      std::vector<SampleType> types;
      types.push_back(SAMPLE_MONOCRYSTAL);

      return types;
    }

  /**
   * @brief Create a new reflection.
   * @return The created Reflection.
   */

  hkl::Sample * SampleFactory::create(const std::string & name, hkl::SampleType type) const throw(hkl::HKLException)
  {
    Sample * sample;

    switch (type)
      {
      case SAMPLE_MONOCRYSTAL :
        sample = new sample::MonoCrystal(_geometry, name);
        break;
      default :
        HKLEXCEPTION("Unknown sample Type.", "Please use a correct type.");
      }
    return sample;
  }


} // namespace hkl
