#ifndef _REFLECTIONFACTORY_H
#define _REFLECTIONFACTORY_H


#include "reflection.h"
#include "HKLException.h"

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class Reflection;
}

namespace hkl
  {

  class ReflectionFactory
    {
    protected:
      hkl::Geometry & _geometry;

      hkl::ReflectionType _type;


    public:
      /**
       * @brief The default constructor.
       * @param geometry the Geometry use to fill the Reflection._geometry.
       * @param type the type of the Reflection.
       */

      ReflectionFactory(hkl::Geometry & geometry, hkl::ReflectionType type);

      /**
       * @brief Create a new reflection.
       * @return The created Reflection.
       */

      hkl::Reflection * create() const throw(hkl::HKLException);

    };

} // namespace hkl
#endif
