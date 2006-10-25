#include "reflectionfactory_monocrystal.h"
#include "reflection_monocrystal.h"

using namespace std;

namespace hkl {
    namespace reflectionfactory {

        MonoCrystal::MonoCrystal(Geometry & geometry) :
          ReflectionFactory(geometry)
        {}

        MonoCrystal::~MonoCrystal(void)
        {}

        Reflection *
        MonoCrystal::create(void)
          {
            return new reflection::MonoCrystal(_geometry, 0, 0, 0, Best, true);
          }

    } // namespace reflectionfactory
} // namespace hkl
