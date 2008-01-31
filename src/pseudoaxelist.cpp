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

#include "pseudoaxelist.h"
#include "pseudoaxe.h"

namespace hkl
  {

  void PseudoAxeList::push_back(hkl::PseudoAxe * pseudoAxe) throw(hkl::HKLException)
  {
    PseudoAxeList::iterator iter = _pseudoAxes.begin();
    PseudoAxeList::iterator end = _pseudoAxes.end();
    while ( iter != end )
      {
        if ((*iter)->get_name() == pseudoAxe->get_name())
          HKLEXCEPTION("Can not add two times the same pseudoAxe", "Change the name of the axe.");
        ++iter;
      }
    _pseudoAxes.push_back(pseudoAxe);
  }

  PseudoAxeList::iterator PseudoAxeList::begin()
  {
    return _pseudoAxes.begin();
  }

  PseudoAxeList::iterator PseudoAxeList::end()
  {
    return _pseudoAxes.end();
  }

  PseudoAxeList::const_iterator PseudoAxeList::begin() const
    {
      return _pseudoAxes.begin();
    }

  PseudoAxeList::const_iterator PseudoAxeList::end() const
    {
      return _pseudoAxes.end();
    }

  /**
   * @brief Get all the names of the PseudoAxes in the PseudoAxeList
   */
  std::vector<std::string> PseudoAxeList::get_names() const
    {
      std::vector<std::string> names;
      PseudoAxeList::const_iterator iter = _pseudoAxes.begin();
      PseudoAxeList::const_iterator end = _pseudoAxes.end();
      while (iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }

      return names;
    }

  /**
   * @brief Get an element of the PseudoAxeList.
   * @param name The name of the PseudoAxe to find.
   * @return A pointer on the PseudoAxe or NULL if the pseudoAxe is not present in the PseudoAxeList
   */
  hkl::PseudoAxe * PseudoAxeList::operator[](const std::string & name)
  {
    PseudoAxeList::iterator iter = _pseudoAxes.begin();
    PseudoAxeList::iterator end = _pseudoAxes.end();
    while (iter != end)
      {
        if ((*iter)->get_name() == name)
          return *iter;
        ++iter;
      }
    return NULL;
  }

  /**
   * @brief Get the size of the PseudoAxeList.
   * @return the number of element in the PseudoAxeList.
   */
  unsigned int PseudoAxeList::size() const
    {
      return _pseudoAxes.size();
    }

  void PseudoAxeList::clear()
  {
    _pseudoAxes.clear();
  }

  /*!
   * \brief print the PseudoAxeList into a flux
   * \param flux The stream to print into.
   */
  std::ostream & PseudoAxeList::printToStream(std::ostream & flux) const
    {
      const_iterator iter = _pseudoAxes.begin();
      const_iterator end = _pseudoAxes.end();
      while (iter != end)
        {
          flux << " PseudoAxe : " << **iter << std::endl;
          ++iter;
        }
      return flux;
    }


} // namespace hkl
