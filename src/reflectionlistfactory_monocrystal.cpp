#include "reflectionlistfactory_monocrystal.h"
#include "reflectionfactory_monocrystal.h"

using namespace std;

namespace hkl {
  namespace reflectionlistfactory {

    MonoCrystal::MonoCrystal(Geometry & geometry) :
      ReflectionListFactory(geometry)
    {
      _reflectionFactory = new reflectionfactory::MonoCrystal(_geometry); 
    }

    MonoCrystal::MonoCrystal(MonoCrystal const & factory) :
      ReflectionListFactory(factory)
    {
      _reflectionFactory = new reflectionfactory::MonoCrystal(_geometry);
    }

    MonoCrystal::~MonoCrystal(void)
    {
      delete _reflectionFactory;
    }

    ReflectionListFactory *
      MonoCrystal::clone(void) const
      {
        return new MonoCrystal(*this);
      }

  } // namespace reflectionlistfactory
} // namespace hkl
