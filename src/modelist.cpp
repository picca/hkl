
#include "modelist.h"
#include "mode.h"

namespace hkl {

/**
 * @brief Default constructor of the ModeList class.
 */

ModeList::ModeList() :
  _current(NULL) 
{
  // Bouml preserved body begin 00034902
  // Bouml preserved body end 00034902
}

/**
 * @brief The default destructor.
 */

ModeList::~ModeList() 
{
  // Bouml preserved body begin 00034982
      ModeList::iterator iter = _modes.begin();
      ModeList::iterator end = _modes.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _modes.clear();
      _current = NULL;
  // Bouml preserved body end 00034982
}

/**
 * @brief Add a mode to the ModeList.
 * @param mode The hkl::Mode to add.
 * @return NULL if the hkl::Mode can not be add or a Pointer on the added hkl::Mode
 */
hkl::Mode * ModeList::add(hkl::Mode * mode) 
{
  // Bouml preserved body begin 00034B02
      //check if a mode with the same name is present in the ModeList
      std::string name = mode->get_name();
      
      ModeList::iterator it = _modes.begin();
      ModeList::iterator end = _modes.end();
      while(it != end)
        {
          if ( (*it)->get_name() == name )
            return NULL;
          ++it;
        }
      _modes.push_back(mode);
      return mode;
  // Bouml preserved body end 00034B02
}

/**
 * @brief Remove a Mode from the ModeList.
 * @param pos The ModeList::iterator position of the Sample.
 * @throw HKLException If the sample is not present. 
 */
ModeList::iterator ModeList::erase(ModeList::iterator & pos) 
{
  // Bouml preserved body begin 00034C02
      Mode * mode_to_erase = *pos;
      if ( mode_to_erase == _current )
        _current = NULL;
      delete mode_to_erase;
      return _modes.erase(pos);
  // Bouml preserved body end 00034C02
}

/**
 * @brief Remove all sample from the SampleList.
 */
void ModeList::clear() 
{
  // Bouml preserved body begin 00034C82
      ModeList::iterator iter = _modes.begin();
      ModeList::iterator end = _modes.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _modes.clear();
      _current = NULL;
  // Bouml preserved body end 00034C82
}

/**
 * @brief Set the nth Mode as the current Mode.
 * @param name The name of the Mode to set as current.
 * @return NULL if the mode is not present in the list but do not change the _current.
 */
hkl::Mode * ModeList::set_current(const std::string & name) 
{
  // Bouml preserved body begin 00034D02
      ModeList::iterator iter = _modes.begin();
      ModeList::iterator end = _modes.end();
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
  // Bouml preserved body end 00034D02
}

/**
 * @brief Get the current Mode
 * @return A pointer on the current Mode.
 */
hkl::Mode * ModeList::get_current() const 
{
  // Bouml preserved body begin 00034D82
      return _current;
  // Bouml preserved body end 00034D82
}

/**
 * @brief Get the current sample
 * @return A pointer on the current sample.
 */
hkl::Mode * ModeList::current() 
{
  // Bouml preserved body begin 00034E02
      return _current;
  // Bouml preserved body end 00034E02
}

/**
 * @brief Return the names of all samples.
 */

std::vector<std::string> ModeList::get_names() const 
{
  // Bouml preserved body begin 00034E82
      std::vector<std::string> names;
      
      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while(iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
  // Bouml preserved body end 00034E82
}

unsigned int ModeList::size() const 
{
  // Bouml preserved body begin 00034F02
      return _modes.size();
  // Bouml preserved body end 00034F02
}

/**
 * @return the Mode * named
 * @param name The name of the Mode we are looking for in the ModeList.
 * @return The mode or NULL if the mode is not present in the ModeList.
 */
hkl::Mode * ModeList::operator[](const std::string & name) 
{
  // Bouml preserved body begin 00034F82
      ModeList::iterator iter = _modes.begin();
      ModeList::iterator end = _modes.end();
      while(iter != end)
        {
          if ( (*iter)->get_name() == name )
            {
              return *iter;
            }
          ++iter;
        }
      return NULL;
  // Bouml preserved body end 00034F82
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

ModeList::iterator ModeList::begin() 
{
  // Bouml preserved body begin 00035002
      return _modes.begin();
  // Bouml preserved body end 00035002
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

ModeList::iterator ModeList::end() 
{
  // Bouml preserved body begin 00035082
      return _modes.end();
  // Bouml preserved body end 00035082
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

ModeList::const_iterator ModeList::begin() const 
{
  // Bouml preserved body begin 00035102
      return _modes.begin();
  // Bouml preserved body end 00035102
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

ModeList::const_iterator ModeList::end() const 
{
  // Bouml preserved body begin 00035182
      return _modes.end();
  // Bouml preserved body end 00035182
}

/**
 * \brief Are two ModeList equals ?
 * \param modeList the hkl::ModeList to compare with.
 * \return true if both are equals flase otherwise.
 */
bool ModeList::operator==(const hkl::ModeList & modeList) const 
{
  // Bouml preserved body begin 00035202
      if (size() != modeList.size())
        return false;
      else
        {
          ModeList::const_iterator iter = _modes.begin();
          ModeList::const_iterator end = _modes.end();
          ModeList::const_iterator iter2 = modeList.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 00035202
}

/**
 * @brief print the ModeList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & ModeList::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00035282
      flux << " ModeList : " << _modes.size() << std::endl;
      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00035282
}

/**
 * @brief print on a stream the content of the ModeList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & ModeList::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00035302
      flux << " " << _modes.size();
      ModeList::const_iterator iter = _modes.begin();
      ModeList::const_iterator end = _modes.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00035302
}

/**
 * @brief restore the content of the ModeList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & ModeList::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00035382
      unsigned int size;
      flux >> size;
      ModeList::iterator iter = _modes.begin();
      for(unsigned int i=0;i<size; i++)
        {
          (*iter)->fromStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00035382
}


} // namespace hkl
