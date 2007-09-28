#include "axefactory.h"
#include "axe.h"

namespace hkl
  {

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
    hkl::Axe * axe = NULL;
    hkl_svector v = {{1, 1, 1}};

    switch (type)
      {
      case AXE_ROTATION :
        axe = new hkl::axe::Rotation(name, "rotation", -M_PI, 0, M_PI, &v);
        break;
      default :
        HKLEXCEPTION("Unknown axe Type.", "Please use a correct type.");
      }
    return axe;
  }


} // namespace hkl
