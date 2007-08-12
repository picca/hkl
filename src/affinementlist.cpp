
#include "affinementlist.h"
#include "affinement.h"

namespace hkl {

/**
 * @brief Default constructor of the AffinementList class.
 */

AffinementList::AffinementList() 
{
  // Bouml preserved body begin 00036882
  // Bouml preserved body end 00036882
}

/**
 * @brief The default destructor.
 */

AffinementList::~AffinementList() 
{
  // Bouml preserved body begin 00036902
      AffinementList::iterator iter = _affinements.begin();
      AffinementList::iterator end = _affinements.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _affinements.clear();
      _current = NULL;
  // Bouml preserved body end 00036902
}

/**
 * @brief Add a mode to the AffinementList.
 * @param affinement The hkl::Affinement to add.
 * @return NULL if the hkl::Affinement can not be add or a Pointer on the added hkl::Affinement
 */
hkl::Affinement * AffinementList::add(hkl::Affinement * affinement) 
{
  // Bouml preserved body begin 00036982
      //check if a mode with the same name is present in the ModeList
      std::string name = affinement->get_name();
      
      AffinementList::iterator it = _affinements.begin();
      AffinementList::iterator end = _affinements.end();
      while(it != end)
        {
          if ( (*it)->get_name() == name )
            return NULL;
          ++it;
        }
      _affinements.push_back(affinement);
      return affinement;
  // Bouml preserved body end 00036982
}

/**
 * @brief Remove a Mode from the AffinementList.
 * @param pos The hkl::AffinementList::iterator position of the Sample.
 * @throw HKLException If the sample is not present. 
 */
hkl::AffinementList::iterator AffinementList::erase(hkl::AffinementList::iterator & pos) 
{
  // Bouml preserved body begin 00036A02
      Affinement * affinement_to_erase = *pos;
      if ( affinement_to_erase == _current )
        _current = NULL;
      delete affinement_to_erase;
      return _affinements.erase(pos);
  // Bouml preserved body end 00036A02
}

/**
 * @brief Remove all sample from the SampleList.
 */
void AffinementList::clear() 
{
  // Bouml preserved body begin 00036A82
      AffinementList::iterator iter = _affinements.begin();
      AffinementList::iterator end = _affinements.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _affinements.clear();
      _current = NULL;
  // Bouml preserved body end 00036A82
}

/**
 * @brief Set the nth Mode as the current Mode.
 * @param name The name of the Mode to set as current.
 * @return NULL if the mode is not present in the list but do not change the _current.
 */
hkl::Affinement * AffinementList::set_current(const std::string & name) 
{
  // Bouml preserved body begin 00036B02
      AffinementList::iterator iter = _affinements.begin();
      AffinementList::iterator end = _affinements.end();
      while(iter != end)
        {
          if ((*iter)->get_name() == name)
            {
              _current = *iter;
              return _current;
            }
          ++iter;
        }
      return NULL;
  // Bouml preserved body end 00036B02
}

/**
 * @brief Get the current Mode
 * @return A pointer on the current Mode.
 */
hkl::Affinement * AffinementList::get_current() const 
{
  // Bouml preserved body begin 00036B82
      return _current;
  // Bouml preserved body end 00036B82
}

/**
 * @brief Get the current sample
 * @return A pointer on the current sample.
 */
hkl::Affinement * AffinementList::current() 
{
  // Bouml preserved body begin 00036C02
      return _current;
  // Bouml preserved body end 00036C02
}

/**
 * @brief Return the names of all samples.
 */

vector<string> AffinementList::get_names() const 
{
  // Bouml preserved body begin 00036C82
      vector<string> names;
      
      AffinementList::const_iterator iter = _affinements.begin();
      AffinementList::const_iterator end = _affinements.end();
      while(iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
  // Bouml preserved body end 00036C82
}

unsigned int AffinementList::size() const 
{
  // Bouml preserved body begin 00036D02
      return _affinements.size();
  // Bouml preserved body end 00036D02
}

/**
 * @return the Mode * named
 * @param name The name of the Mode we are looking for in the AffinementList.
 * @return The mode or NULL if the mode is not present in the AffinementList.
 */
hkl::Affinement * AffinementList::operator[](const std::string & name) 
{
  // Bouml preserved body begin 00036D82
      AffinementList::iterator iter = _affinements.begin();
      AffinementList::iterator end = _affinements.end();
      while(iter != end)
        {
          if ( (*iter)->get_name() == name )
            {
              return *iter;
            }
        }
      return NULL;
  // Bouml preserved body end 00036D82
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

hkl::AffinementList::iterator AffinementList::begin() 
{
  // Bouml preserved body begin 00036E02
      return _affinements.begin();
  // Bouml preserved body end 00036E02
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

hkl::AffinementList::iterator AffinementList::end() 
{
  // Bouml preserved body begin 00036E82
      return _affinements.end();
  // Bouml preserved body end 00036E82
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

hkl::AffinementList::const_iterator AffinementList::begin() const 
{
  // Bouml preserved body begin 00036F02
      return _affinements.begin();
  // Bouml preserved body end 00036F02
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

hkl::AffinementList::const_iterator AffinementList::end() const 
{
  // Bouml preserved body begin 00036F82
      return _affinements.end();
  // Bouml preserved body end 00036F82
}

/**
 * \brief Are two AffinementList equals ?
 * \param affinementList the hkl::AffinementList to compare with.
 * \return true if both are equals flase otherwise.
 */
bool AffinementList::operator==(const hkl::AffinementList & affinementList) const 
{
  // Bouml preserved body begin 00037002
      if (size() != affinementList.size())
        return false;
      else
        {
          AffinementList::const_iterator iter = _affinements.begin();
          AffinementList::const_iterator end = _affinements.end();
          AffinementList::const_iterator iter2 = affinementList.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 00037002
}

/**
 * @brief print the AffinementList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & AffinementList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00037082
      flux << " AffinementList : " << _affinements.size() << endl;
      AffinementList::const_iterator iter = _affinements.begin();
      AffinementList::const_iterator end = _affinements.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00037082
}

/**
 * @brief print on a stream the content of the AffinementList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & AffinementList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00037102
      flux << " " << _affinements.size();
      AffinementList::const_iterator iter = _affinements.begin();
      AffinementList::const_iterator end = _affinements.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00037102
}

/**
 * @brief restore the content of the AffinementList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & AffinementList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00037182
      unsigned int size;
      flux >> size;
      AffinementList::iterator iter = _affinements.begin();
      for(unsigned int i=0;i<size; i++)
        {
          (*iter)->fromStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00037182
}


} // namespace hkl
