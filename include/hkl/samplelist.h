#ifndef _SAMPLELIST_H_
#define _SAMPLELIST_H_

#include "samplefactory.h"

using namespace std;

namespace hkl
  {

  class SampleList : public vector<Sample *>
    {
    public:
      /**
       * @brief The default constructor
       * @param geometry The Geometry use to create the Reflections store in the samples.
       */
      SampleList(Geometry & geometry);

      /**
       * @brief The copy constructor.
       * @param sampleList The SampleList to copy.
       */
      SampleList(SampleList const & sampleList);

      /**
       * @brief The default destructor.
       */
      ~SampleList(void);

      /**
       * @brief Get a list of all Sample type available.
       * @return A vector fill with all available sample type.
       */
      vector<SampleType> types(void) const;

      /**
       * @brief Add a Sample to the SampleList.
       * @param name The name of the Sample
       * @param type The type of the Sample to add
       * @throw HKLException if a sample with the same name is already present in the list. 
       */
      void add(MyString const & name, SampleType type) throw (HKLException);

      /**
       * @brief add a copy of a sample
       * @param pos An iterator on the Sample to copy.
       */
      void add_copy(vector<Sample *>::iterator pos);

      /**
      * @brief Remove a sample from the SampleList.
      * @param pos the position of the Sample.
      * @throw HKLException If the sample is not present. 
      */
      void erase(vector<Sample *>::iterator pos) throw (HKLException);

      /**
      * @brief Remove all sample from the SampleList.
      */
      void clear(void);

      /**
       * @brief Set the nth sample as the current sample.
       * @param index The index of the sample to set as current.
       * @throw HKLException if the index is out of range.
       */
      void set_current(unsigned int index) throw (HKLException);

      /**
       * @brief Set the nth sample as the current sample.
       * @param name The name of the sample to set as current.
       * @throw HKLException if the index is out of range.
       */
      void set_current(MyString const & name) throw (HKLException);

      /**
       * @brief Get the current sample
       * @return A pointer on the current sample.
       */
      Sample * current(void) throw (HKLException);

      /**
       * @brief Are two SampleList equals ?
       * @param sampleList the SampleList to compare with.
       * @return True if both are equals, false otherwise.
       */
      bool operator==(SampleList const & sampleList) const;

      /**
       * @brief print the SampleList into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Save the SampleList into a stream.
       * @param flux the stream to save the SampleList into.
       * @return The stream with the SampleList.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore an SampleList from a stream.
       * @param flux The stream containing the SampleList.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

    private:
      Sample * _current; //!< The current Sample.
      //vector<Sample *> _samples; //!< A vector with all the samples.
      Geometry & _geometry; //!< The Geometry use for calculation.
      SampleFactory * _samplefactory; //!< The sample factory use to create the Samples.
    };
} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::SampleList const & sampleList)
{
  return sampleList.printToStream(flux);
}

#endif // _SAMPLELIST_H_
