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

#include "parameter.h"
#include "value.h"

namespace hkl
  {

  /**
   * @brief The default constructor
   * @param name The name std::string of the Parameter.
   * @param description The description std::string of the Parameter.
   * @param min the minimum hkl::Value of the Parameter.
   * @param current The current hkl::Value of the Parameter.
   * @param max The maximum hkl::Value of the Parameter.
   * @throw HKLException if the min <= current <= max is not verify.
   */
  Parameter::Parameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException) :
      ObjectReadOnly(name, description),
      Range(min, current, current, max)
  {
  }

  /*!
   * \brief Are two Parameter equals ?
   * \param parameter the hkl::Parameter to compare with.
   */

  bool Parameter::operator==(const hkl::Parameter & parameter) const
    {
      return ObjectReadOnly::operator==(parameter)
             && Range::operator==(parameter);
    }

  /*!
   * \brief print the Parameter into a flux
   * \param flux The stream to print into.
   */
  std::ostream & Parameter::printToStream(std::ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      flux << " ";
      Range::printToStream(flux);
      return flux;
    }

  /*!
   * \brief Save the Parameter into a stream.
   * \param flux the stream to save the Parameter into.
   * \return The stream with the Parameter.
   */
  std::ostream & Parameter::toStream(std::ostream & flux) const
    {
      ObjectReadOnly::toStream(flux);
      Range::toStream(flux);
      return flux;
    }

  /*!
   * \brief Restore a Parameter from a stream.
   * \param flux The stream containing the Parameter to restore.
   * @todo call update_observers or not ?
   */
  std::istream & Parameter::fromStream(std::istream & flux)
  {
    ObjectReadOnly::fromStream(flux);
    Range::fromStream(flux);
    return flux;
  }


} // namespace hkl
