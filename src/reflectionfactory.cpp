#include "reflectionfactory.h"
#include "reflection_monocrystal.h"
#include "geometry.h"

extern struct hkl_svector hkl_svector_X;

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
    static hkl_svector hkl = {{1,0,0}};

    Reflection * reflection;

    switch (_type)
      {
      case REFLECTION_MONOCRYSTAL :
        reflection = new reflection::MonoCrystal(_geometry, &hkl, true);
        break;
      default :
        HKLEXCEPTION("Unknown reflection Type.", "Please use a correct type.");
      }
    return reflection;
  }


} // namespace hkl
