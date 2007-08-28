
#include "pseudoaxeengine.h"
#include "pseudoaxe.h"

namespace hkl {

PseudoAxeEngine::PseudoAxeEngine() :
  HKLObject("engine", "engine") 
{
  // Bouml preserved body begin 00030202
  // Bouml preserved body end 00030202
}

/**
 * @brief The default destructor.
 */
PseudoAxeEngine::~PseudoAxeEngine() 
{
  // Bouml preserved body begin 00030282
  // Bouml preserved body end 00030282
}

/**
 * @brief Set the read part of a PseudoAxe without calling the set methode of the engine
 * @param pseudoAxe the hkl::PseudoAxe to set
 * @param min the minimum value to set
 * @param current the current value to set
 * @param consign the consign value to set
 * @param max The maximum value to set
 */
void PseudoAxeEngine::set_pseudoAxe(hkl::PseudoAxe * pseudoAxe, double min, double current, double consign, double & max) 
{
  // Bouml preserved body begin 0003EC82
  pseudoAxe->_min.set_value(min);
  pseudoAxe->_current.set_value(current);
  pseudoAxe->_consign.set_value(consign);
  pseudoAxe->_max.set_value(max);
  // Bouml preserved body end 0003EC82
}


} // namespace hkl
