#include "reflectionfactory.h"

using namespace std;

namespace hkl {

    ReflectionFactory::ReflectionFactory(Geometry & geometry) :
      _geometry(geometry)
    {}

    ReflectionFactory::~ReflectionFactory(void)
      {}

} // namespace hkl
