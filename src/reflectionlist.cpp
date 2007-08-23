
#include "reflectionlist.h"
#include "geometry.h"
#include "reflectionfactory.h"
#include "reflection.h"
#include "svector.h"

namespace hkl {

/**
 * @brief Default constructor
 * @param geometry The Geometry related to the Reflection 
 * @param type The type of the Reflection in the ReflectionList.
 */

ReflectionList::ReflectionList(hkl::Geometry & geometry, hkl::ReflectionType type) :
  _geometry(geometry) 
{
  // Bouml preserved body begin 0002D682
      _reflectionFactory = new ReflectionFactory(_geometry, type);
  // Bouml preserved body end 0002D682
}

/**
 * @brief The default destructor.
 */

ReflectionList::~ReflectionList() 
{
  // Bouml preserved body begin 0002D702
      delete _reflectionFactory;
      
      std::vector<Reflection *>::iterator iter = _reflections.begin();
      std::vector<Reflection *>::iterator end = _reflections.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
  // Bouml preserved body end 0002D702
}

/**
 * @brief The copy constructor.
 * @param factory The factory to copy from.
 */

ReflectionList::ReflectionList(const hkl::ReflectionList & source) :
  _geometry(source._geometry)
{
  // Bouml preserved body begin 0002D782
      _reflectionFactory = new ReflectionFactory(*(source._reflectionFactory));
      
      std::vector<Reflection *>::const_iterator iter = source._reflections.begin();
      std::vector<Reflection *>::const_iterator end = source._reflections.end();
      while(iter != end)
        {
          _reflections.push_back((*iter)->clone());
          ++iter;
        }
  // Bouml preserved body end 0002D782
}

/**
 * @brief Make a deep copy of a ReflectionList.
 * 
 * @return A pointer on the copied ReflectionList.
 */

hkl::ReflectionList * ReflectionList::clone() const 
{
  // Bouml preserved body begin 0002D802
      return new ReflectionList(*this);
  // Bouml preserved body end 0002D802
}

/**
 * @brief Add a reflection to the ReflectionList.
 * @param hkl The scattering vector of the added reflection.
 * @return A reference on the added reflection.
 */

hkl::Reflection & ReflectionList::add(const hkl::svector & hkl) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002D882
      Reflection * reflection = _reflectionFactory->create();
      reflection->set_hkl(hkl);
      
      // When trying to add an active reflection, check that the reflection is not already in.
      // if already in change the flag to false.
      if (reflection->flag())
        {
          std::vector<Reflection *>::iterator iter = _reflections.begin();
          std::vector<Reflection *>::iterator end = _reflections.end();
          while(iter != end)
            {
              if (hkl == (*iter)->get_hkl())
                {
                  reflection->flag() = false;
                }
              ++iter;
            }
        }
      
      // add the reflection
      _reflections.push_back(reflection);
      
      return *reflection;
  // Bouml preserved body end 0002D882
}

/**
 * @brief Delete the ith reflection
 * @param index of the reflection to delete.
 * @throw HKLException if index is out of range.
 */

void ReflectionList::del(unsigned int index) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002D902
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
          for(unsigned int i=0;i<index;i++)
            ++iter;
          delete *iter;
          _reflections.erase(iter);
        }
  // Bouml preserved body end 0002D902
}

/**
 * @brief Return the number of reflection in the ReflectionList.
 * @return The number of reflection in the ReflectionList.
 */

unsigned int ReflectionList::size() const 
{
  // Bouml preserved body begin 0002D982
      return _reflections.size();
  // Bouml preserved body end 0002D982
}

/**
 * @brief Return the number of undependant Reflection in the ReflectionList.
 * 
 * @return The number of non-colinear Reflection in the ReflectionList.
 */

unsigned int ReflectionList::size_indep() const 
{
  // Bouml preserved body begin 0002DA02
      unsigned int nb_usable_reflections = 0;
      std::vector<Reflection *>::const_iterator iter = _reflections.begin();
      std::vector<Reflection *>::const_iterator iter2 = _reflections.begin();
      std::vector<Reflection *>::const_iterator end = _reflections.end();
      
      while(iter < end)
        {
          if ((*iter)->flag())
            {
              if (nb_usable_reflections == 0)
                nb_usable_reflections = 1;
              iter2 = iter;
              ++iter2;
              while(iter2 < end)
                {
                  if ((*iter2)->flag() && !(*iter)->isColinear(**iter2))
                    nb_usable_reflections++;
                  ++iter2;
                }
            }
          ++iter;
        }
      return nb_usable_reflections;
  // Bouml preserved body end 0002DA02
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
  // Bouml preserved body begin 0002DA82
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
  // Bouml preserved body end 0002DA82
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

ReflectionList::iterator ReflectionList::begin() 
{
  // Bouml preserved body begin 0002DB02
      return _reflections.begin();
  // Bouml preserved body end 0002DB02
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

ReflectionList::iterator ReflectionList::end() 
{
  // Bouml preserved body begin 0002DB82
      return _reflections.end();
  // Bouml preserved body end 0002DB82
}

/**
 * \brief Are two ReflectionList equals ?
 * \param reflectionList the hkl::ReflectionList to compare with.
 * \return true if both are equals flase otherwise.
 */
bool ReflectionList::operator==(const hkl::ReflectionList & reflectionList) const 
{
  // Bouml preserved body begin 0002DC02
      if (!(_geometry == reflectionList._geometry))
        return false;
      
      if (_reflections.size() != reflectionList._reflections.size())
        return false;
      else
        {
          std::vector<Reflection *>::const_iterator iter = _reflections.begin();
          std::vector<Reflection *>::const_iterator end = _reflections.end();
          std::vector<Reflection *>::const_iterator iter2 = reflectionList._reflections.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
        }
      return true;
  // Bouml preserved body end 0002DC02
}

/**
 * @brief print the ReflectionList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & ReflectionList::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0002DC82
      _geometry.printToStream(flux);
      
      flux << _reflections.size() << " reflection(s)" << std::endl;
      std::vector<Reflection *>::const_iterator iter = _reflections.begin();
      std::vector<Reflection *>::const_iterator end = _reflections.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          flux << std::endl;
          ++iter;
        }
      return flux;
  // Bouml preserved body end 0002DC82
}

/**
 * @brief print on a stream the content of the ReflectionList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & ReflectionList::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0002DD02
      unsigned int nb_reflections = _reflections.size();
      
      flux << nb_reflections << std::endl;
      for(unsigned int i=0;i<nb_reflections;i++)
        _reflections[i]->toStream(flux);
      return flux;
  // Bouml preserved body end 0002DD02
}

/**
 * @brief restore the content of the ReflectionList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & ReflectionList::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 0002DD82
      unsigned int nb_reflections = _reflections.size();
      if ( nb_reflections )
        {
          std::vector<Reflection *>::iterator iter = _reflections.begin();
          std::vector<Reflection *>::iterator end = _reflections.end();
          while(iter != end)
            {
              delete *iter;
            }
          _reflections.clear();
        }
      
      flux >> nb_reflections;
      for(unsigned int i=0; i< nb_reflections; i++)
        {
          Reflection * reflection = _reflectionFactory->create();
          reflection->fromStream(flux);
          _reflections.push_back(reflection);
        }
      return flux;
  // Bouml preserved body end 0002DD82
}


} // namespace hkl
