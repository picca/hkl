
#include "affinementlist.h"
#include "affinement.h"

namespace hkl
  {

  /**
   * @brief Default constructor of the AffinementList class.
   */

  AffinementList::AffinementList()
  {
  }

  /**
   * @brief The default destructor.
   */

  AffinementList::~AffinementList()
  {
    AffinementList::iterator iter = _affinements.begin();
    AffinementList::iterator end = _affinements.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _affinements.clear();
    _current = NULL;
  }

  /**
   * @brief Add a mode to the AffinementList.
   * @param affinement The hkl::Affinement to add.
   * @return NULL if the hkl::Affinement can not be add or a Pointer on the added hkl::Affinement
   */
  hkl::Affinement * AffinementList::add(hkl::Affinement * affinement)
  {
    //check if a mode with the same name is present in the ModeList
    std::string name = affinement->get_name();

    AffinementList::iterator it = _affinements.begin();
    AffinementList::iterator end = _affinements.end();
    while (it != end)
      {
        if ( (*it)->get_name() == name )
          return NULL;
        ++it;
      }
    _affinements.push_back(affinement);
    return affinement;
  }

  /**
   * @brief Remove a Mode from the AffinementList.
   * @param pos The hkl::AffinementList::iterator position of the Sample.
   * @throw HKLException If the sample is not present.
   */
  hkl::AffinementList::iterator AffinementList::erase(hkl::AffinementList::iterator & pos)
  {
    Affinement * affinement_to_erase = *pos;
    if ( affinement_to_erase == _current )
      _current = NULL;
    delete affinement_to_erase;
    return _affinements.erase(pos);
  }

  /**
   * @brief Remove all sample from the SampleList.
   */
  void AffinementList::clear()
  {
    AffinementList::iterator iter = _affinements.begin();
    AffinementList::iterator end = _affinements.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _affinements.clear();
    _current = NULL;
  }

  /**
   * @brief Set the nth Mode as the current Mode.
   * @param name The name of the Mode to set as current.
   * @return NULL if the mode is not present in the list but do not change the _current.
   */
  hkl::Affinement * AffinementList::set_current(const std::string & name)
  {
    AffinementList::iterator iter = _affinements.begin();
    AffinementList::iterator end = _affinements.end();
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
  hkl::Affinement * AffinementList::get_current() const
    {
      return _current;
    }

  /**
   * @brief Get the current sample
   * @return A pointer on the current sample.
   */
  hkl::Affinement * AffinementList::current()
  {
    return _current;
  }

  /**
   * @brief Return the names of all samples.
   */

  std::vector<std::string> AffinementList::get_names() const
    {
      std::vector<std::string> names;

      AffinementList::const_iterator iter = _affinements.begin();
      AffinementList::const_iterator end = _affinements.end();
      while (iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
    }

  unsigned int AffinementList::size() const
    {
      return _affinements.size();
    }

  /**
   * @return the Mode * named
   * @param name The name of the Mode we are looking for in the AffinementList.
   * @return The mode or NULL if the mode is not present in the AffinementList.
   */
  hkl::Affinement * AffinementList::operator[](const std::string & name)
  {
    AffinementList::iterator iter = _affinements.begin();
    AffinementList::iterator end = _affinements.end();
    while (iter != end)
      {
        if ( (*iter)->get_name() == name )
          {
            return *iter;
          }
      }
    return NULL;
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  hkl::AffinementList::iterator AffinementList::begin()
  {
    return _affinements.begin();
  }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  hkl::AffinementList::iterator AffinementList::end()
  {
    return _affinements.end();
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  hkl::AffinementList::const_iterator AffinementList::begin() const
    {
      return _affinements.begin();
    }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  hkl::AffinementList::const_iterator AffinementList::end() const
    {
      return _affinements.end();
    }

  /**
   * \brief Are two AffinementList equals ?
   * \param affinementList the hkl::AffinementList to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool AffinementList::operator==(const hkl::AffinementList & affinementList) const
    {
      if (size() != affinementList.size())
        return false;
      else
        {
          AffinementList::const_iterator iter = _affinements.begin();
          AffinementList::const_iterator end = _affinements.end();
          AffinementList::const_iterator iter2 = affinementList.begin();
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
   * @brief print the AffinementList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & AffinementList::printToStream(std::ostream & flux) const
    {
      flux << " AffinementList : " << _affinements.size() << std::endl;
      AffinementList::const_iterator iter = _affinements.begin();
      AffinementList::const_iterator end = _affinements.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

} // namespace hkl
