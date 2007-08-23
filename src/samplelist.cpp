
#include "samplelist.h"
#include "geometry.h"
#include "sample.h"
#include "samplefactory.h"

namespace hkl {

/**
 * @brief Default constructor
 * @param geometry The Geometry related to the Reflection.
 */

SampleList::SampleList(hkl::Geometry & geometry) :
  _geometry(geometry),
  _current(NULL) 
{
  // Bouml preserved body begin 0002EF82
      _samplefactory = new SampleFactory(geometry);
  // Bouml preserved body end 0002EF82
}

/**
 * @brief The default destructor.
 */

SampleList::~SampleList() 
{
  // Bouml preserved body begin 0002F002
      SampleList::iterator iter = _samples.begin();
      SampleList::iterator end = _samples.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _samples.clear();
      
      delete _samplefactory;
  // Bouml preserved body end 0002F002
}

/**
 * @brief The copy constructor.
 * @param source The hkl::SampleList to copy from.
 */

SampleList::SampleList(const hkl::SampleList & source) :
  _geometry(source._geometry) 
{
  // Bouml preserved body begin 0002F082
      _samplefactory = new SampleFactory(_geometry);
      
      SampleList::const_iterator iter = source.begin();
      SampleList::const_iterator end = source.end();
      while(iter != end)
        {
          _samples.push_back((*iter)->clone());
          ++iter;
        }
      set_current(source.get_current()->get_name());
  // Bouml preserved body end 0002F082
}

/**
 * @brief Get a list of all Sample type available.
 * @return A vector fill with all available sample type.
 */
std::vector<SampleType> SampleList::types() const 
{
  // Bouml preserved body begin 0002F702
      return _samplefactory->types();
  // Bouml preserved body end 0002F702
}

/**
 * @brief Add a Sample to the SampleList.
 * @param name The name of the Sample
 * @param type The type of the Sample to add
 * @throw HKLException if a sample with the same name is already present in the list. 
 */
hkl::Sample * SampleList::add(const std::string & name, hkl::SampleType type) 
{
  // Bouml preserved body begin 0002F182
      //check if a sample with the same name is present in the samplelist
      SampleList::iterator it = _samples.begin();
      SampleList::iterator end = _samples.end();
      while(it != end)
        {
          if ( (*it)->get_name() == name )
            return NULL;
          ++it;
        }
      Sample * sample = _samplefactory->create(name, type);
      _samples.push_back(sample);
      return sample;
  // Bouml preserved body end 0002F182
}

/**
 * @brief add a copy of a sample
 * @param pos An iterator on the Sample to copy.
 */
hkl::Sample * SampleList::add_copy(hkl::SampleList::const_iterator & pos) 
{
  // Bouml preserved body begin 0002F782
      Sample * sample_to_copy = *pos;
      std::string name = sample_to_copy->get_name() + "_copy";
      
      //check if a sample with the same name is present in the samplelist
      SampleList::iterator it = _samples.begin();
      SampleList::iterator end = _samples.end();
      while(it != end)
        {
          if ( (*it)->get_name() == name )
            return NULL;
          ++it;
        }
      Sample * sample = sample_to_copy->clone();
      sample->set_name(name);
      _samples.push_back(sample);
      return sample;
  // Bouml preserved body end 0002F782
}

/**
 * @brief Remove a sample from the SampleList.
 * @param pos the position of the Sample.
 * @throw HKLException If the sample is not present. 
 */
void SampleList::erase(hkl::SampleList::iterator & pos) 
{
  // Bouml preserved body begin 0002F802
      delete *pos;
      
      // update the _current.
      iterator iter = _samples.erase(pos);
      if (iter == _samples.end())
          _current = NULL;
      else
          _current = *iter;
  // Bouml preserved body end 0002F802
}

/**
 * @brief Remove all sample from the SampleList.
 */
void SampleList::clear() 
{
  // Bouml preserved body begin 0002F882
      SampleList::iterator iter = _samples.begin();
      SampleList::iterator end = _samples.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _samples.clear();
      _current = NULL;
  // Bouml preserved body end 0002F882
}

/**
 * @brief Set the nth sample as the current sample.
 * @param name The name of the sample to set as current.
 * @throw HKLException if the index is out of range.
 */
hkl::Sample * SampleList::set_current(const std::string & name) 
{
  // Bouml preserved body begin 0002F102
      SampleList::iterator iter = _samples.begin();
      SampleList::iterator end = _samples.end();
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
  // Bouml preserved body end 0002F102
}

/**
 * @brief Get the current sample
 * @return A pointer on the current sample.
 */
hkl::Sample * SampleList::get_current() const 
{
  // Bouml preserved body begin 0002FA82
      return _current;
  // Bouml preserved body end 0002FA82
}

/**
 * @brief Get the current sample
 * @return A pointer on the current sample.
 */
hkl::Sample * SampleList::current() 
{
  // Bouml preserved body begin 0002F202
      return _current;
  // Bouml preserved body end 0002F202
}

/**
 * @brief Return the names of all samples.
 */

std::vector<std::string> SampleList::get_names() const 
{
  // Bouml preserved body begin 0002F282
      std::vector<std::string> names;
      
      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while(iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
  // Bouml preserved body end 0002F282
}

unsigned int SampleList::size() const 
{
  // Bouml preserved body begin 0002F902
      return _samples.size();
  // Bouml preserved body end 0002F902
}

hkl::Sample * SampleList::operator[](const std::string & name) 
{
  // Bouml preserved body begin 0002F382
      SampleList::iterator iter = _samples.begin();
      SampleList::iterator end = _samples.end();
      while(iter != end)
        {
          if ( (*iter)->get_name() == name )
            {
              return *iter;
            }
        }
      return NULL;
  // Bouml preserved body end 0002F382
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

SampleList::iterator SampleList::begin() 
{
  // Bouml preserved body begin 0002F402
      return _samples.begin();
  // Bouml preserved body end 0002F402
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

SampleList::iterator SampleList::end() 
{
  // Bouml preserved body begin 0002F482
      return _samples.end();
  // Bouml preserved body end 0002F482
}

/**
 * @brief Get an iterator on the first element of ReflectionList.
 * @return The iterator.
 */

SampleList::const_iterator SampleList::begin() const 
{
  // Bouml preserved body begin 0002F982
      return _samples.begin();
  // Bouml preserved body end 0002F982
}

/**
 * @brief Get an iterator on the end of ReflectionList.
 * @return The iterator.
 */

SampleList::const_iterator SampleList::end() const 
{
  // Bouml preserved body begin 0002FA02
      return _samples.end();
  // Bouml preserved body end 0002FA02
}

/**
 * \brief Are two SampleList equals ?
 * \param sampleList the hkl::SampleList to compare with.
 * \return true if both are equals flase otherwise.
 */
bool SampleList::operator==(const hkl::SampleList & sampleList) const 
{
  // Bouml preserved body begin 0002F502
      if (size() != sampleList.size())
        return false;
      else
        {
          SampleList::const_iterator iter = _samples.begin();
          SampleList::const_iterator end = _samples.end();
          SampleList::const_iterator iter2 = sampleList.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 0002F502
}

/**
 * @brief print the SampleList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & SampleList::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0002F582
      flux << " SampleList : " << _samples.size() << std::endl;
      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 0002F582
}

/**
 * @brief print on a stream the content of the SampleList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & SampleList::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0002F602
      flux << " " << _samples.size();
      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while(iter != end)
        {
          flux << " " << (*iter)->get_type();
          (*iter)->toStream(flux);
          ++iter;
        }
      // save the current crystal name.
      MyString current_name("no current");
      if (_current)
          current_name = _current->get_name();
      current_name.toStream(flux);
      return flux;
  // Bouml preserved body end 0002F602
}

/**
 * @brief restore the content of the SampleList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & SampleList::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 0002F682
      // remove all samples before restoring
      clear();
      
      unsigned int size;
      int type;
      flux >> size;
      for(unsigned int i=0;i<size; i++)
        {
          flux >> type;
          Sample * sample = _samplefactory->create("fromstream", (SampleType)type);
          sample->fromStream(flux);
          _samples.push_back(sample);
        }
      MyString current_name;
      current_name.fromStream(flux);
      if (current_name == "no current")
        _current = NULL;
      else
        set_current(current_name);
      return flux;
  // Bouml preserved body end 0002F682
}


} // namespace hkl
