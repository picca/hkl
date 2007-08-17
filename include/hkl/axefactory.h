#ifndef _HKL_AXEFACTORY_H
#define _HKL_AXEFACTORY_H


#include "axe.h"
#include <string>

#include "HKLException.h"
#include "axe_rotation.h"

namespace hkl { class Axe; } 

namespace hkl {

class AxeFactory {
  public:
    /**
     * @brief Create a new reflection.
     * @param type The type of the Axe to create.
     * @param name The name of the Axe to add.
     * @return The created Reflection.
     * 
     * This method is only use in the holderList fromStream Method.
     */
    static hkl::Axe * create(hkl::AxeType type, const std::string & name) throw(hkl::HKLException);

};

} // namespace hkl
#endif
