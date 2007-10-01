#ifndef _SAMPLELIST_H
#define _SAMPLELIST_H


#include <vector>
#include "sample.h"
#include <string>
#include <ostream>
#include <istream>

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class Sample;
}
namespace hkl
  {
  class SampleFactory;
}

namespace hkl
  {

  class SampleList
    {
    protected:
      hkl::Geometry & _geometry;

      hkl::Sample * _current;

      hkl::SampleFactory * _samplefactory;

      std::vector<hkl::Sample *> _samples;


    public:
      typedef std::vector<Sample *>::iterator iterator;

      typedef std::vector<Sample *>::const_iterator const_iterator;

      /**
       * @brief Default constructor
       * @param geometry The Geometry related to the Reflection.
       */

      SampleList(hkl::Geometry & geometry);

      /**
       * @brief The default destructor.
       */

      virtual ~SampleList();

      /**
       * @brief The copy constructor.
       * @param source The SampleList to copy from.
       */

      SampleList(const SampleList & source);

      /**
       * @brief Get a list of all Sample type available.
       * @return A vector fill with all available sample type.
       */
      std::vector<SampleType> types() const;

      /**
       * @brief Add a Sample to the SampleList.
       * @param name The name of the Sample
       * @param type The type of the Sample to add
       * @throw HKLException if a sample with the same name is already present in the list.
       */
      hkl::Sample * add(const std::string & name, hkl::SampleType type);

      /**
       * @brief add a copy of a sample
       * @param pos An iterator on the Sample to copy.
       */
      hkl::Sample * add_copy(const_iterator & pos);

      /**
       * @brief Remove a sample from the SampleList.
       * @param pos the position of the Sample.
       * @throw HKLException If the sample is not present.
       */
      void erase(iterator & pos);

      /**
       * @brief Remove all sample from the SampleList.
       */
      void clear();

      /**
       * @brief Set the nth sample as the current sample.
       * @param name The name of the sample to set as current.
       * @throw HKLException if the index is out of range.
       */
      hkl::Sample * set_current(const std::string & name);

      /**
       * @brief Get the current sample
       * @return A pointer on the current sample.
       */
      hkl::Sample * get_current() const;

      /**
       * @brief Get the current sample
       * @return A pointer on the current sample.
       */
      hkl::Sample * current();

      /**
       * @brief Return the names of all samples.
       */

      std::vector<std::string> get_names() const;

      unsigned int size() const;

      hkl::Sample * operator[](const std::string & name);

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */

      SampleList::iterator begin();

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */

      SampleList::iterator end();

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */

      SampleList::const_iterator begin() const;

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */

      SampleList::const_iterator end() const;

      /**
       * \brief Are two SampleList equals ?
       * \param sampleList the SampleList to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const SampleList & sampleList) const;

      /**
       * @brief print the SampleList into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };

} // namespace hkl

inline std::ostream &
operator <<(std::ostream & flux, hkl::SampleList const & sampleList)
{
  return sampleList.printToStream(flux);
}
#endif
