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

#include "hklobject.h"

namespace hkl
  {

  /**
   * @brief The default constructor
   * @param name The name of the HKLObject.
   * @param description The description of the HKLObject.
   * @throw HKLException if the name and/or the description are wrong.
   */
  HKLObject::HKLObject(const std::string & name, const std::string & description) throw(hkl::HKLException):
      ObjectReadOnly(name, description)
  {
  }

  hkl::ParameterList & HKLObject::parameters()
  {
    return _parameters;
  }

  /**
   * \brief Are two HKLObject equals ?
   * \param hklObject the hkl::HKLObject to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool HKLObject::operator==(const hkl::HKLObject & hklObject) const
    {
      return ObjectReadOnly::operator==(hklObject)
             && _parameters == hklObject._parameters;
    }

  /**
   * @brief print the HKLObject into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & HKLObject::printToStream(std::ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      flux << std::endl << _parameters;

      return flux;
    }

  /**
   * @brief print on a stream the content of the HKLObject
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & HKLObject::toStream(std::ostream & flux) const
    {
      ObjectReadOnly::toStream(flux);
      _parameters.toStream(flux);

      return flux;
    }

  /**
   * @brief restore the content of the HKLObject from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & HKLObject::fromStream(std::istream & flux)
  {
    ObjectReadOnly::fromStream(flux);
    _parameters.fromStream(flux);

    return flux;
  }


} // namespace hkl
