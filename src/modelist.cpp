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

#include "modelist.h"
#include "mode.h"

namespace hkl
  {

  /**
   * @brief Default constructor of the ModeList class.
   */

  ModeList::ModeList() :
      _current(NULL)
  {
  }

  /**
   * @brief The default destructor.
   */

  ModeList::~ModeList()
  {
    ModeList::iterator iter = _modes.begin();
    ModeList::iterator end = _modes.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _modes.clear();
    _current = NULL;
  }

  /**
   * @brief Add a mode to the ModeList.
   * @param mode The hkl::Mode to add.
   * @return NULL if the hkl::Mode can not be add or a Pointer on the added hkl::Mode
   */
  hkl::Mode * ModeList::add(hkl::Mode * mode)
  {
    //check if a mode with the same name is present in the ModeList
    std::string name = mode->get_name();

    ModeList::iterator it = _modes.begin();
    ModeList::iterator end = _modes.end();
    while (it != end)
      {
        if ( (*it)->get_name() == name )
          return NULL;
        ++it;
      }
    _modes.push_back(mode);
    return mode;
  }

  /**
   * @brief Remove a Mode from the ModeList.
   * @param pos The ModeList::iterator position of the Sample.
   * @throw HKLException If the sample is not present.
   */
  ModeList::iterator ModeList::erase(ModeList::iterator & pos)
  {
    Mode * mode_to_erase = *pos;
    if ( mode_to_erase == _current )
      _current = NULL;
    delete mode_to_erase;
    return _modes.erase(pos);
  }

  /**
   * @brief Remove all sample from the SampleList.
   */
  void ModeList::clear()
  {
    ModeList::iterator iter = _modes.begin();
    ModeList::iterator end = _modes.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _modes.clear();
    _current = NULL;
  }

  /**
   * @brief Set the nth Mode as the current Mode.
   * @param name The name of the Mode to set as current.
   * @return NULL if the mode is not present in the list but do not change the _current.
   */
  hkl::Mode * ModeList::set_current(const std::string & name)
  {
    ModeList::iterator iter = _modes.begin();
    ModeList::iterator end = _modes.end();
    while (iter != end)
      {
        if ((*iter)->get_name() == name)
          {
            _current = *iter;
            return _current;
          }
        ++iter;
      }
    return NULL;
  }

  /**
   * @brief Get the current Mode
   * @return A pointer on the current Mode.
   */
  hkl::Mode * ModeList::get_current() const
    {
      return _current;
    }

  /**
   * @brief Get the current sample
   * @return A pointer on the current sample.
   */
  hkl::Mode * ModeList::current()
  {
    return _current;
  }

  /**
   * @brief Return the names of all samples.
   */

  std::vector<std::string> ModeList::get_names() const
    {
      std::vector<std::string> names;

      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while (iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
    }

  unsigned int ModeList::size() const
    {
      return _modes.size();
    }

  /**
   * @return the Mode * named
   * @param name The name of the Mode we are looking for in the ModeList.
   * @return The mode or NULL if the mode is not present in the ModeList.
   */
  hkl::Mode * ModeList::operator[](const std::string & name)
  {
    ModeList::iterator iter = _modes.begin();
    ModeList::iterator end = _modes.end();
    while (iter != end)
      {
        if ( (*iter)->get_name() == name )
          {
            return *iter;
          }
        ++iter;
      }
    return NULL;
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  ModeList::iterator ModeList::begin()
  {
    return _modes.begin();
  }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  ModeList::iterator ModeList::end()
  {
    return _modes.end();
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  ModeList::const_iterator ModeList::begin() const
    {
      return _modes.begin();
    }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  ModeList::const_iterator ModeList::end() const
    {
      return _modes.end();
    }

  /**
   * \brief Are two ModeList equals ?
   * \param modeList the hkl::ModeList to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool ModeList::operator==(const hkl::ModeList & modeList) const
    {
      if (size() != modeList.size())
        return false;
      else
        {
          ModeList::const_iterator iter = _modes.begin();
          ModeList::const_iterator end = _modes.end();
          ModeList::const_iterator iter2 = modeList.begin();
          while (iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
    }

  /**
   * @brief print the ModeList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & ModeList::printToStream(std::ostream & flux) const
    {
      flux << " ModeList : " << _modes.size() << std::endl;
      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  /**
   * @brief print on a stream the content of the ModeList
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & ModeList::toStream(std::ostream & flux) const
    {
      flux << " " << _modes.size();
      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while (iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  /**
   * @brief restore the content of the ModeList from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & ModeList::fromStream(std::istream & flux)
  {
    unsigned int size;
    flux >> size;
    ModeList::iterator iter = _modes.begin();
    for (unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }


} // namespace hkl
