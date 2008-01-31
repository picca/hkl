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
#ifndef _PSEUDOAXELIST_H
#define _PSEUDOAXELIST_H


#include <vector>
#include "HKLException.h"
#include <string>
#include <ostream>

namespace hkl
  {
  class PseudoAxe;
}

namespace hkl
  {

  class PseudoAxeList
    {
    protected:
      std::vector<hkl::PseudoAxe *> _pseudoAxes;


    public:
      typedef std::vector<hkl::PseudoAxe*>::iterator iterator;

      typedef std::vector<hkl::PseudoAxe*>::const_iterator const_iterator;

      void push_back(hkl::PseudoAxe * pseudoAxe) throw(hkl::HKLException);

      PseudoAxeList::iterator begin();

      PseudoAxeList::iterator end();

      PseudoAxeList::const_iterator begin() const;

      PseudoAxeList::const_iterator end() const;

      /**
       * @brief Get all the names of the PseudoAxes in the PseudoAxeList
       */
      std::vector<std::string> get_names() const;

      /**
       * @brief Get an element of the PseudoAxeList.
       * @param name The name of the PseudoAxe to find.
       * @return A pointer on the PseudoAxe or NULL if the pseudoAxe is not present in the PseudoAxeList
       */
      hkl::PseudoAxe * operator[](const std::string & name);

      /**
       * @brief Get the size of the PseudoAxeList.
       * @return the number of element in the PseudoAxeList.
       */
      unsigned int size() const;

      void clear();

      /*!
       * \brief print the PseudoAxeList into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };

} // namespace hkl
/*!
 * @brief Overload of the << operator for the %PseudoAxeList class
 * @param flux
 * @param pseudoAxeList
 * @return the modified flux.
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::PseudoAxeList const & pseudoAxeList)
{
  return pseudoAxeList.printToStream(flux);
}
#endif
