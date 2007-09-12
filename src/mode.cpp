
#include "mode.h"

namespace hkl
  {

  /**
   * @brief The default constructor of the Mode class.
   * @param name the name of the Mode.
   * @param description the description of the Mode.
   */

  Mode::Mode(const std::string & name, const std::string & description) :
      HKLObject(name, description)
  {
  }

  Mode::~Mode()
  {
  }


} // namespace hkl
