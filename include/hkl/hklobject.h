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
#ifndef _HKLOBJECT_H
#define _HKLOBJECT_H


#include "object.h"
#include "parameterlist.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {

  class HKLObject : public hkl::ObjectReadOnly
    {
    protected:
      hkl::ParameterList _parameters;


    public:
      /**
       * @brief The default constructor
       * @param name The name of the HKLObject.
       * @param description The description of the HKLObject.
       * @throw HKLException if the name and/or the description are wrong.
       */
      HKLObject(const std::string & name, const std::string & description) throw(hkl::HKLException);

      hkl::ParameterList & parameters();

      /**
       * \brief Are two HKLObject equals ?
       * \param hklObject the HKLObject to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const HKLObject & hklObject) const;

      /**
       * @brief print the HKLObject into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the HKLObject
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the HKLObject from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl
/*!
 * @brief Overload of the << operator for the HKLObject class
 * @param flux
 * @param hklObject
 * @return the modified flux.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::HKLObject const & hklObject)
{
  return hklObject.printToStream(flux);
}

#endif
