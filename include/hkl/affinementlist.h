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
#ifndef _AFFINEMENTLIST_H
#define _AFFINEMENTLIST_H


#include <vector>
#include <string>
#include <ostream>
#include <istream>

namespace hkl
  {
  class Affinement;
}

namespace hkl
  {

  class AffinementList
    {
    protected:
      hkl::Affinement * _current;

      std::vector<hkl::Affinement *> _affinements;


    public:
      typedef std::vector<Affinement *>::iterator iterator;

      typedef std::vector<Affinement *>::const_iterator const_iterator;

      /**
       * @brief Default constructor of the AffinementList class.
       */

      AffinementList();

      /**
       * @brief The default destructor.
       */

      virtual ~AffinementList();

      /**
       * @brief Add a mode to the AffinementList.
       * @param affinement The hkl::Affinement to add.
       * @return NULL if the hkl::Affinement can not be add or a Pointer on the added hkl::Affinement
       */
      hkl::Affinement * add(hkl::Affinement * affinement);

      /**
       * @brief Remove a Mode from the AffinementList.
       * @param pos The iterator position of the Sample.
       * @throw HKLException If the sample is not present.
       */
      iterator erase(iterator & pos);

      /**
       * @brief Remove all sample from the SampleList.
       */
      void clear();

      /**
       * @brief Set the nth Mode as the current Mode.
       * @param name The name of the Mode to set as current.
       * @return NULL if the mode is not present in the list but do not change the _current.
       */
      hkl::Affinement * set_current(const std::string & name);

      /**
       * @brief Get the current Mode
       * @return A pointer on the current Mode.
       */
      hkl::Affinement * get_current() const;

      /**
       * @brief Get the current sample
       * @return A pointer on the current sample.
       */
      hkl::Affinement * current();

      /**
       * @brief Return the names of all samples.
       */

      std::vector<std::string> get_names() const;

      unsigned int size() const;

      /**
       * @return the Mode * named
       * @param name The name of the Mode we are looking for in the AffinementList.
       * @return The mode or NULL if the mode is not present in the AffinementList.
       */
      hkl::Affinement * operator[](const std::string & name);

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */

      iterator begin();

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */

      iterator end();

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */

      const_iterator begin() const;

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */

      const_iterator end() const;

      /**
       * \brief Are two AffinementList equals ?
       * \param affinementList the AffinementList to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const AffinementList & affinementList) const;

      /**
       * @brief print the AffinementList into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the AffinementList
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the AffinementList from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl
inline std::ostream &
operator <<(std::ostream & flux, hkl::AffinementList const & affinementList)
{
  return affinementList.printToStream(flux);
}

#endif
