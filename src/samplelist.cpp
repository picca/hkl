
#include "samplelist.h"
#include "geometry.h"
#include "sample.h"
#include "samplefactory.h"

namespace hkl
  {

  /**
   * @brief Default constructor
   * @param geometry The Geometry related to the Reflection.
   */

  SampleList::SampleList(hkl::Geometry & geometry) :
      _geometry(geometry),
      _current(NULL)
  {
    _samplefactory = new SampleFactory(geometry);
  }

  /**
   * @brief The default destructor.
   */

  SampleList::~SampleList()
  {
    SampleList::iterator iter = _samples.begin();
    SampleList::iterator end = _samples.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _samples.clear();

    delete _samplefactory;
  }

  /**
   * @brief The copy constructor.
   * @param source The hkl::SampleList to copy from.
   */

  SampleList::SampleList(const hkl::SampleList & source) :
      _geometry(source._geometry)
  {
    _samplefactory = new SampleFactory(_geometry);

    SampleList::const_iterator iter = source.begin();
    SampleList::const_iterator end = source.end();
    while (iter != end)
      {
        _samples.push_back((*iter)->clone());
        ++iter;
      }
    set_current(source.get_current()->get_name());
  }

  /**
   * @brief Get a list of all Sample type available.
   * @return A vector fill with all available sample type.
   */
  std::vector<SampleType> SampleList::types() const
    {
      return _samplefactory->types();
    }

  /**
   * @brief Add a Sample to the SampleList.
   * @param name The name of the Sample
   * @param type The type of the Sample to add
   * @throw HKLException if a sample with the same name is already present in the list.
   */
  hkl::Sample * SampleList::add(const std::string & name, hkl::SampleType type)
  {
    //check if a sample with the same name is present in the samplelist
    SampleList::iterator it = _samples.begin();
    SampleList::iterator end = _samples.end();
    while (it != end)
      {
        if ( (*it)->get_name() == name )
          return NULL;
        ++it;
      }
    Sample * sample = _samplefactory->create(name, type);
    _samples.push_back(sample);
    return sample;
  }

  /**
   * @brief add a copy of a sample
   * @param pos An iterator on the Sample to copy.
   */
  hkl::Sample * SampleList::add_copy(hkl::SampleList::const_iterator & pos)
  {
    Sample * sample_to_copy = *pos;
    std::string name = sample_to_copy->get_name() + "_copy";

    //check if a sample with the same name is present in the samplelist
    SampleList::iterator it = _samples.begin();
    SampleList::iterator end = _samples.end();
    while (it != end)
      {
        if ( (*it)->get_name() == name )
          return NULL;
        ++it;
      }
    Sample * sample = sample_to_copy->clone();
    sample->set_name(name);
    _samples.push_back(sample);
    return sample;
  }

  /**
   * @brief Remove a sample from the SampleList.
   * @param pos the position of the Sample.
   * @throw HKLException If the sample is not present.
   */
  void SampleList::erase(hkl::SampleList::iterator & pos)
  {
    delete *pos;

    // update the _current.
    iterator iter = _samples.erase(pos);
    if (iter == _samples.end())
      _current = NULL;
    else
      _current = *iter;
  }

  /**
   * @brief Remove all sample from the SampleList.
   */
  void SampleList::clear()
  {
    SampleList::iterator iter = _samples.begin();
    SampleList::iterator end = _samples.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _samples.clear();
    _current = NULL;
  }

  /**
   * @brief Set the nth sample as the current sample.
   * @param name The name of the sample to set as current.
   * @throw HKLException if the index is out of range.
   */
  hkl::Sample * SampleList::set_current(const std::string & name)
  {
    SampleList::iterator iter = _samples.begin();
    SampleList::iterator end = _samples.end();
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
   * @brief Get the current sample
   * @return A pointer on the current sample.
   */
  hkl::Sample * SampleList::get_current() const
    {
      return _current;
    }

  /**
   * @brief Get the current sample
   * @return A pointer on the current sample.
   */
  hkl::Sample * SampleList::current()
  {
    return _current;
  }

  /**
   * @brief Return the names of all samples.
   */

  std::vector<std::string> SampleList::get_names() const
    {
      std::vector<std::string> names;

      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while (iter != end)
        {
          names.push_back((*iter)->get_name());
          ++iter;
        }
      return names;
    }

  unsigned int SampleList::size() const
    {
      return _samples.size();
    }

  hkl::Sample * SampleList::operator[](const std::string & name)
  {
    SampleList::iterator iter = _samples.begin();
    SampleList::iterator end = _samples.end();
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

  SampleList::iterator SampleList::begin()
  {
    return _samples.begin();
  }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  SampleList::iterator SampleList::end()
  {
    return _samples.end();
  }

  /**
   * @brief Get an iterator on the first element of ReflectionList.
   * @return The iterator.
   */

  SampleList::const_iterator SampleList::begin() const
    {
      return _samples.begin();
    }

  /**
   * @brief Get an iterator on the end of ReflectionList.
   * @return The iterator.
   */

  SampleList::const_iterator SampleList::end() const
    {
      return _samples.end();
    }

  /**
   * \brief Are two SampleList equals ?
   * \param sampleList the hkl::SampleList to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool SampleList::operator==(const hkl::SampleList & sampleList) const
    {
      if (size() != sampleList.size())
        return false;
      else
        {
          SampleList::const_iterator iter = _samples.begin();
          SampleList::const_iterator end = _samples.end();
          SampleList::const_iterator iter2 = sampleList.begin();
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
   * @brief print the SampleList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & SampleList::printToStream(std::ostream & flux) const
    {
      flux << " SampleList : " << _samples.size() << std::endl;
      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  /**
   * @brief print on a stream the content of the SampleList
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & SampleList::toStream(std::ostream & flux) const
    {
      flux << " " << _samples.size();
      SampleList::const_iterator iter = _samples.begin();
      SampleList::const_iterator end = _samples.end();
      while (iter != end)
        {
          flux << " " << (*iter)->get_type();
          (*iter)->toStream(flux);
          ++iter;
        }
      // save the current crystal name.
      if (_current)
        strbuf_to_stream(_current->get_name(), flux);
      else
        strbuf_to_stream("no current", flux);

      return flux;
    }

  /**
   * @brief restore the content of the SampleList from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & SampleList::fromStream(std::istream & flux)
  {
    // remove all samples before restoring
    clear();

    unsigned int size;
    int type;
    flux >> size;
    for (unsigned int i=0;i<size; i++)
      {
        flux >> type;
        Sample * sample = _samplefactory->create("fromstream", (SampleType)type);
        sample->fromStream(flux);
        _samples.push_back(sample);
      }
    std::string current_name;
    strbuf_from_stream(current_name, flux);
    if (current_name == "no current")
      _current = NULL;
    else
      this->set_current(current_name);
    return flux;
  }


} // namespace hkl
