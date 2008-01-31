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
#ifndef _PARAMETERLIST_H
#define _PARAMETERLIST_H


#include <vector>
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class Parameter;
}

namespace hkl
  {

  class ParameterList
    {
    protected:
      std::vector<hkl::Parameter *> _parameters;


    public:
      typedef std::vector<Parameter *>::iterator iterator;

      typedef std::vector<Parameter *>::const_iterator const_iterator;

      /**
       * @brief Add a hkl::Parameter to the ParameterList.
       * @param parameter The hkl::Parameter to add.
       */
      bool add(hkl::Parameter * parameter);

      /**
       * @brief Get the size of the ParameterList.
       * @return the number of element in the ParameterList.
       */
      unsigned int size() const;

      std::vector<std::string> get_names() const;

      /**
       * @return the std::string * named
       * @param name The name of the std::string we are looking for in the ParameterList.
       * @return A std::string pointer.
       * @throw HKLException if the std::string is not present n the ParameterList.
       */
      hkl::Parameter * operator[](const std::string & name) throw(hkl::HKLException);

      /**
       * @brief Get an iterator on the first element of the ParameterList.
       * @return The iterator.
       */
      iterator begin();

      /**
       * @brief Get an iterator on the end of the ParameterList.
       * @return The iterator.
       */
      iterator end();

      /**
       * @brief Get an const_iterator on the first element of the ParameterList.
       * @return The const_iterator.
       */
      const_iterator begin() const;

      /**
       * @brief Get an const_iterator on the end of the ParameterList.
       * @return The const_iterator.
       */
      const_iterator end() const;

      /*!
       * \brief Are two ParameterList equals ?
       * \param parameterList the ParameterList to compare with.
       */

      bool operator==(const ParameterList & parameterList) const;

      /*!
       * \brief print the ParameterList into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the ParameterList into a stream.
       * \param flux the stream to save the ParameterList into.
       * \return The stream with the ParameterList.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a ParameterList from a stream.
       * \param flux The stream containing the ParameterList to restore.
       * @todo call update_observers or not ?
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl
/*!
 * @brief Overload of the << operator for the %ParameterList class
 * @param flux
 * @param parameterList
 * @return the modified flux.
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::ParameterList const & parameterList)
{
  return parameterList.printToStream(flux);
}

#endif
