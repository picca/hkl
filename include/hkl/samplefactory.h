#ifndef _SAMPLEFACTORY_H_
#define _SAMPLEFACTORY_H_

#include "sample.h"

using namespace std;

namespace hkl {

    class SampleFactory
      {
      public:

        SampleFactory(Geometry & geometry);

        virtual ~SampleFactory(void);

        vector<SampleType> types(void) const;

        Sample * create(MyString const & name, SampleType type) const;

      protected:

        Geometry & _geometry;
      };

} // namespace hkl

#endif // _SAMPLEFACTORY_H_
