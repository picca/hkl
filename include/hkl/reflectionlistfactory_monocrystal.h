#ifndef _REFLECTIONLISTFACTORY_MONOCRYSTAL_H_
#define _REFLECTIONLISTFACTORY_MONOCRYSTAL_H_

#include "reflectionlistfactory.h"

using namespace std;

namespace hkl {
    namespace reflectionlistfactory {

        class MonoCrystal : public ReflectionListFactory
        {
        public:

          MonoCrystal(Geometry & geometry);

          MonoCrystal(MonoCrystal const & factory);

          ReflectionListFactory * clone(void) const;

          virtual ~MonoCrystal(void);

        };

    } // namespace reflectionlistfactory
} // namespace hkl

#endif // _MONOCRYSTALREFLECTIONLISTFACTORY_H_
