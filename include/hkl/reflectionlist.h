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
#ifndef _REFLECTIONLIST_H
#define _REFLECTIONLIST_H


#include <vector>
#include "reflection.h"
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class ReflectionFactory;
}
namespace hkl
  {
  class Reflection;
}
namespace hkl
  {
  class svector;
}

namespace hkl
  {

  class ReflectionList
    {
    protected:
      hkl::Geometry & _geometry;

      hkl::ReflectionFactory * _reflectionFactory;

      std::vector<hkl::Reflection *> _reflections;


    public:
      typedef std::vector<Reflection *>::iterator iterator;

      /**
       * @brief Default constructor
       * @param geometry The Geometry related to the Reflection
       * @param type The type of the Reflection in the ReflectionList.
       */

      ReflectionList(hkl::Geometry & geometry, hkl::ReflectionType type);

      /**
       * @brief The default destructor.
       */

      virtual ~ReflectionList();

      /**
       * @brief The copy constructor.
       * @param factory The factory to copy from.
       */

      ReflectionList(const ReflectionList & source);

      /**
       * @brief Make a deep copy of a ReflectionList.
       *
       * @return A pointer on the copied ReflectionList.
       */

      virtual ReflectionList * clone() const;

      /**
       * @brief Add a reflection to the ReflectionList.
       * @param hkl The scattering vector of the added reflection.
       * @return A reference on the added reflection.
       */

      hkl::Reflection & add(const hkl::svector & hkl) throw(hkl::HKLException);

      /**
       * @brief Delete the ith reflection
       * @param index of the reflection to delete.
       * @throw HKLException if index is out of range.
       */

      void del(unsigned int index) throw(hkl::HKLException);

      /**
       * @brief Return the number of reflection in the ReflectionList.
       * @return The number of reflection in the ReflectionList.
       */

      unsigned int size() const;

      /**
       * @brief Return the number of undependant Reflection in the ReflectionList.
       *
       * @return The number of non-colinear Reflection in the ReflectionList.
       */

      unsigned int size_indep() const;

      /**
       * @brief Return a reference on the ReflectionList ith Reflection.
       *
       * @param index of the returned Reflection.
       * @throw HKLException if index is out of range.
       *
       * @return The ith Reflection.
       */

      hkl::Reflection * operator[](unsigned int index) throw(hkl::HKLException);

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */

      ReflectionList::iterator begin();

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */

      ReflectionList::iterator end();

      /**
       * \brief Are two ReflectionList equals ?
       * \param reflectionList the ReflectionList to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const ReflectionList & reflectionList) const;

      /**
       * @brief print the ReflectionList into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the ReflectionList
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the ReflectionList from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl

inline std::ostream &
operator<<(std::ostream & flux, hkl::ReflectionList const & factory)
{
  return factory.printToStream(flux);
}

#endif
