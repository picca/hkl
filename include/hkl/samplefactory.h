#ifndef _SAMPLEFACTORY_H
#define _SAMPLEFACTORY_H


#include <vector>
#include "sample.h"
#include <string>
#include "HKLException.h"

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

  class SampleFactory
    {
    protected:
      hkl::Geometry & _geometry;


    public:
      /**
       * @brief The default constructor.
       * @param geometry the Geometry use to fill the Reflection._geometry.
       */

      SampleFactory(hkl::Geometry & geometry);

      std::vector<SampleType> types() const;

      /**
       * @brief Create a new reflection.
       * @return The created Reflection.
       */

      hkl::Sample * create(const std::string & name, hkl::SampleType type) const throw(hkl::HKLException);

    };

} // namespace hkl
#endif
