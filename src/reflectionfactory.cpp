#include "reflectionfactory.h"
#include "reflection_monocrystal.h"

using namespace std;

namespace hkl
  {

  ReflectionFactory::ReflectionFactory(Geometry & geometry, ReflectionType const & type) :
      _geometry(geometry),
      _type(type)
  {}

  ReflectionFactory::~ReflectionFactory(void)
  {}

  Reflection *
  ReflectionFactory::create(void) throw (HKLException)
  {
    Reflection * reflection;

    switch (_type)
      {
      case REFLECTION_MONOCRYSTAL :
        reflection = new reflection::MonoCrystal(_geometry, svector(), true);
        break;
      default :
        HKLEXCEPTION("Unknown reflection Type.", "Please use a correct type.");
      }
    return reflection;
  }
} // namespace hkl
