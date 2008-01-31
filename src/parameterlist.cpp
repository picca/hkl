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

#include "parameterlist.h"
#include "parameter.h"

namespace hkl
  {

  /**
   * @brief Add a hkl::Parameter to the ParameterList.
   * @param parameter The hkl::Parameter to add.
   */
  bool ParameterList::add(hkl::Parameter * parameter)
  {
    std::vector<Parameter *>::iterator iter = _parameters.begin();
    std::vector<Parameter *>::iterator end = _parameters.end();
    while (iter != end)
      {
        if ((*iter)->get_name() == parameter->get_name())
          return false;
        ++iter;
      }
    _parameters.push_back(parameter);
    return true;
  }

  /**
   * @brief Get the size of the ParameterList.
   * @return the number of element in the ParameterList.
   */
  unsigned int ParameterList::size() const
    {
      return _parameters.size();
    }

  std::vector<std::string> ParameterList::get_names() const
    {
      std::vector<std::string> names;

      const_iterator iter = _parameters.begin();
      const_iterator end = _parameters.end();
      while (iter != end)
        {
          names.push_back( (*iter)->get_name() );
          ++iter;
        }

      return names;
    }

  /**
   * @return the std::string * named
   * @param name The name of the std::string we are looking for in the ParameterList.
   * @return A std::string pointer.
   * @throw HKLException if the std::string is not present n the ParameterList.
   */
  hkl::Parameter * ParameterList::operator[](const std::string & name) throw(hkl::HKLException)
  {
    std::vector<Parameter *>::iterator iter = _parameters.begin();
    std::vector<Parameter *>::iterator end = _parameters.end();
    while (iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    HKLEXCEPTION("Cannot find this parameter", "Check the name of the parameter.");
  }

  /**
   * @brief Get an iterator on the first element of the ParameterList.
   * @return The iterator.
   */
  hkl::ParameterList::iterator ParameterList::begin()
  {
    return _parameters.begin();
  }

  /**
   * @brief Get an iterator on the end of the ParameterList.
   * @return The iterator.
   */
  hkl::ParameterList::iterator ParameterList::end()
  {
    return _parameters.end();
  }

  /**
   * @brief Get an const_iterator on the first element of the ParameterList.
   * @return The const_iterator.
   */
  hkl::ParameterList::const_iterator ParameterList::begin() const
    {
      return _parameters.begin();
    }

  /**
   * @brief Get an const_iterator on the end of the ParameterList.
   * @return The const_iterator.
   */
  hkl::ParameterList::const_iterator ParameterList::end() const
    {
      return _parameters.end();
    }

  /*!
   * \brief Are two ParameterList equals ?
   * \param parameterList the hkl::ParameterList to compare with.
   */

  bool ParameterList::operator==(const hkl::ParameterList & parameterList) const
    {
      if (_parameters.size() != parameterList._parameters.size())
        return false;
      else
        {
          std::vector<Parameter *>::const_iterator iter = _parameters.begin();
          std::vector<Parameter *>::const_iterator iter2 = parameterList._parameters.begin();
          std::vector<Parameter *>::const_iterator end = _parameters.end();
          while (iter != end)
            {
              if ( !(**iter == **iter2) )
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
    }

  /*!
   * \brief print the ParameterList into a flux
   * \param flux The stream to print into.
   */
  std::ostream & ParameterList::printToStream(std::ostream & flux) const
    {
      std::vector<Parameter *>::const_iterator iter = _parameters.begin();
      std::vector<Parameter *>::const_iterator end = _parameters.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          flux << std::endl;
          ++iter;
        }
      return flux;
    }

  /*!
   * \brief Save the ParameterList into a stream.
   * \param flux the stream to save the ParameterList into.
   * \return The stream with the ParameterList.
   */
  std::ostream & ParameterList::toStream(std::ostream & flux) const
    {
      std::vector<Parameter *>::const_iterator iter = _parameters.begin();
      std::vector<Parameter *>::const_iterator end = _parameters.end();
      while (iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  /*!
   * \brief Restore a ParameterList from a stream.
   * \param flux The stream containing the ParameterList to restore.
   * @todo call update_observers or not ?
   */
  std::istream & ParameterList::fromStream(std::istream & flux)
  {
    std::vector<Parameter *>::iterator iter = _parameters.begin();
    std::vector<Parameter *>::iterator end = _parameters.end();
    while (iter != end)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }


} // namespace hkl
