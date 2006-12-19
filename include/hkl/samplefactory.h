#ifndef _SAMPLEFACTORY_H_
#define _SAMPLEFACTORY_H_

#include "sample.h"

using namespace std;

namespace hkl
  {

  class SampleFactory
    {
    public:

      /**
        * @brief The default constructor
       * @param geometry The Geometry use for the reflections.
       */
      SampleFactory(Geometry & geometry);

      /**
       * @brief The default destructor.
       */
      virtual ~SampleFactory(void);

      /**
       * @brief Get the available types of samples.
       * 
       * @return A vector fill with all available sample types.
       */
      vector<SampleType> types(void) const;

      /**
       * @brief Create a new Sample.
       * 
       * @param name the name of the Sample
       * @param type The type of the sample to create.
       * 
       *  @return A pointer on the new created sample.
       */
      Sample * create(MyString const & name, SampleType type) const;

    protected:

      Geometry & _geometry; //!< A reference on the geometry use to create reflections.
    };

} // namespace hkl

#endif // _SAMPLEFACTORY_H_
