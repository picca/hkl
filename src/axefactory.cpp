
#include "axefactory.h"
#include "axe.h"

namespace hkl {

/**
 * @brief Create a new reflection.
 * @param type The type of the Axe to create.
 * @param name The name of the Axe to add.
 * @return The created Reflection.
 * 
 * This method is only use in the holderList fromStream Method.
 */
hkl::Axe * AxeFactory::create(hkl::AxeType type, const std::string & name) throw(hkl::HKLException)

{
  // Bouml preserved body begin 0003DB02
        hkl::Axe * axe = NULL;
        
        switch (type)
          {
          case AXE_ROTATION :
            axe = new hkl::axe::Rotation(name, "rotation", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(1, 1, 1));
            break;
          default :
            HKLEXCEPTION("Unknown axe Type.", "Please use a correct type.");
          }
        return axe;
  // Bouml preserved body end 0003DB02
}


} // namespace hkl
