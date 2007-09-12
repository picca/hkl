
#include "pseudoaxeengine.h"
#include "pseudoaxe.h"

namespace hkl
  {

  PseudoAxeEngine::PseudoAxeEngine() :
      HKLObject("engine", "engine")
  {
  }

  /**
   * @brief The default destructor.
   */
  PseudoAxeEngine::~PseudoAxeEngine()
  {
  }

  /**
   * @brief Set the read part of a PseudoAxe without calling the set methode of the engine
   * @param pseudoAxe the hkl::PseudoAxe to set
   * @param min the minimum value to set
   * @param current the current value to set
   * @param consign the consign value to set
   * @param max The maximum value to set
   */
  void PseudoAxeEngine::set_pseudoAxe(hkl::PseudoAxe * pseudoAxe, double min, double current, double consign, double max)
  {
    pseudoAxe->_min.set_value(min);
    pseudoAxe->_current.set_value(current);
    pseudoAxe->_consign.set_value(consign);
    pseudoAxe->_max.set_value(max);
  }


} // namespace hkl
