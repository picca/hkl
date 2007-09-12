
#include "reflectionfactory.h"
#include "geometry.h"

#include "reflection_monocrystal.h"
namespace hkl
  {

  /**
   * @brief The default constructor.
   * @param geometry the Geometry use to fill the Reflection._geometry.
   * @param type the type of the Reflection.
   */

  ReflectionFactory::ReflectionFactory(hkl::Geometry & geometry, hkl::ReflectionType type)  :
      _geometry(geometry),
      _type(type)
  {
  }

  /**
   * @brief Create a new reflection.
   * @return The created Reflection.
   */

  hkl::Reflection * ReflectionFactory::create() const throw(hkl::HKLException)
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
