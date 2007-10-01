
#include "reflectionlist.h"
#include "geometry.h"
#include "reflectionfactory.h"
#include "reflection.h"
#include "svector.h"

namespace hkl
  {

  /**
   * @brief Default constructor
   * @param geometry The Geometry related to the Reflection
   * @param type The type of the Reflection in the ReflectionList.
   */

  ReflectionList::ReflectionList(hkl::Geometry & geometry, hkl::ReflectionType type) :
      _geometry(geometry)
  {
    _reflectionFactory = new ReflectionFactory(_geometry, type);
  }

  /**
   * @brief The default destructor.
   */

  ReflectionList::~ReflectionList()
  {
    delete _reflectionFactory;

    std::vector<Reflection *>::iterator iter = _reflections.begin();
    std::vector<Reflection *>::iterator end = _reflections.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
  }

  /**
   * @brief The copy constructor.
   * @param factory The factory to copy from.
   */

  ReflectionList::ReflectionList(const hkl::ReflectionList & source) :
      _geometry(source._geometry)
  {
    _reflectionFactory = new ReflectionFactory(*(source._reflectionFactory));

    std::vector<Reflection *>::const_iterator iter = source._reflections.begin();
    std::vector<Reflection *>::const_iterator end = source._reflections.end();
    while (iter != end)
      {
        _reflections.push_back((*iter)->clone());
        ++iter;
      }
  }

  /**
   * @brief Make a deep copy of a ReflectionList.
   *
   * @return A pointer on the copied ReflectionList.
   */

  hkl::ReflectionList * ReflectionList::clone() const
    {
      return new ReflectionList(*this);
    }

  /**
   * @brief Add a reflection to the ReflectionList.
   * @param hkl The scattering vector of the added reflection.
   * @return A reference on the added reflection.
   */

  hkl::Reflection & ReflectionList::add(hkl_svector const * hkl) throw(hkl::HKLException)
  {
    Reflection * reflection = _reflectionFactory->create();
    reflection->set_hkl(hkl);

    // When trying to add an active reflection, check that the reflection is not already in.
    // if already in change the flag to false.
    if (reflection->flag())
      {
        std::vector<Reflection *>::iterator iter = _reflections.begin();
        std::vector<Reflection *>::iterator end = _reflections.end();
        while (iter != end)
          {
            if (::hkl_svector_cmp(hkl, (*iter)->get_hkl()))
              {
                reflection->flag() = false;
              }
            ++iter;
          }
      }

    // add the reflection
    _reflections.push_back(reflection);

    return *reflection;
  }

  /**
   * @brief Delete the ith reflection
   * @param index of the reflection to delete.
   * @throw HKLException if index is out of range.
   */

  void ReflectionList::del(unsigned int index) throw(hkl::HKLException)
  {
    unsigned int nb_reflection = _reflections.size();

    if (index >= nb_reflection)
      {
        std::ostringstream reason;
        std::ostringstream description;

        reason << "Can not delete the reflection : " << index;
        if (nb_reflection)
          description << "Index out of range, the maximum index is : " << nb_reflection-1;
        else
          description << "There is no reflection to delete.";

        HKLEXCEPTION(reason.str(), description.str());
      }
    else
      {
        std::vector<Reflection *>::iterator iter = _reflections.begin();
        for (unsigned int i=0;i<index;i++)
          ++iter;
        delete *iter;
        _reflections.erase(iter);
      }
  }

  /**
   * @brief Return the number of reflection in the ReflectionList.
   * @return The number of reflection in the ReflectionList.
   */

  unsigned int ReflectionList::size() const
    {
      return _reflections.size();
    }

  /**
   * @brief Return the number of undependant Reflection in the ReflectionList.
   *
   * @return The number of non-colinear Reflection in the ReflectionList.
   */

  unsigned int ReflectionList::size_indep() const
    {
      unsigned int nb_usable_reflections = 0;
      std::vector<Reflection *>::const_iterator iter = _reflections.begin();
      std::vector<Reflection *>::const_iterator iter2 = _reflections.begin();
      std::vector<Reflection *>::const_iterator end = _reflections.end();

      while (iter < end)
        {
          if ((*iter)->flag())
            {
              if (nb_usable_reflections == 0)
                nb_usable_reflections = 1;
              iter2 = iter;
              ++iter2;
              while (iter2 < end)
                {
                  if ((*iter2)->flag() && !(*iter)->isColinear(**iter2))
                    nb_usable_reflections++;
                  ++iter2;
                }
            }
          ++iter;
        }
      return nb_usable_reflections;
    }

  /**
   * @brief Return a reference on the ReflectionList ith Reflection.
   *
   * @param index of the returned Reflection.
   * @throw HKLException if index is out of range.
   *
   * @return The ith Reflection.
   */

  hkl::Reflection * ReflectionList::operator[](unsigned int index) throw(hkl::HKLException)
  {
    unsigned int nb_reflection = _reflections.size();

    if (index >= nb_reflection)
      {
        std::ostringstream reason;
        std::ostringstream description;

        reason << "Index of the reflection is out of range : " << index;
        if (nb_reflection > 1)
          description << "The maximum index is : " << nb_reflection-1;
        else
          description << "No reflection in the ReflectionList";

        HKLEXCEPTION(reason.str(), description.str());
      }
    else
      return _reflections[index];
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  ReflectionList::iterator ReflectionList::begin()
  {
    return _reflections.begin();
  }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  ReflectionList::iterator ReflectionList::end()
  {
    return _reflections.end();
  }

  /**
   * \brief Are two ReflectionList equals ?
   * \param reflectionList the hkl::ReflectionList to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool ReflectionList::operator==(const hkl::ReflectionList & reflectionList) const
    {
      if (!(_geometry == reflectionList._geometry))
        return false;

      if (_reflections.size() != reflectionList._reflections.size())
        return false;
      else
        {
          std::vector<Reflection *>::const_iterator iter = _reflections.begin();
          std::vector<Reflection *>::const_iterator end = _reflections.end();
          std::vector<Reflection *>::const_iterator iter2 = reflectionList._reflections.begin();
          while (iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
        }
      return true;
    }

  /**
   * @brief print the ReflectionList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & ReflectionList::printToStream(std::ostream & flux) const
    {
      _geometry.printToStream(flux);

      flux << _reflections.size() << " reflection(s)" << std::endl;
      std::vector<Reflection *>::const_iterator iter = _reflections.begin();
      std::vector<Reflection *>::const_iterator end = _reflections.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          flux << std::endl;
          ++iter;
        }
      return flux;
    }

} // namespace hkl
