
#include "twoC_vertical_diffractometer.h"
#include "twoC_vertical_mode.h"
#include "twoC_vertical_pseudoaxeengine.h"

namespace hkl {

namespace twoC {

namespace vertical {

Diffractometer::Diffractometer() :
  hkl::DiffractometerTemp<hkl::twoC::vertical::Geometry>("2C Generic Soleil", "This diffractometer was design by Frédéric-emmanuel PICCA picca@synchrotron-soleil.fr\n\
              * modes: \"Symetric\", \"Fix incidence\"\n\
              * pseudoAxes: \"th2th\", \"q2th\", \"q\"")
{
  // Bouml preserved body begin 00037982
      // On met Ã  jour la liste des modes utilisables.
      _modes.add( new hkl::twoC::vertical::mode::Symetric("Symetric", "Omega = 2theta / 2. = theta", _geom_T) );
      _modes.add( new hkl::twoC::vertical::mode::Fix_Incidence("Fix incidence", "2theta = 2 * theta, omega is free.", _geom_T) );
      
      // On ajoute les pseudoAxes
      _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Th2th(_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Q2th(_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::twoC::vertical::pseudoAxeEngine::Q(_geom_T) );
  // Bouml preserved body end 00037982
}

Diffractometer::~Diffractometer() 
{
  // Bouml preserved body begin 00037A02
      // On supprime les modes.
      _modes.clear();
      // On supprime les pseudoAxes.
      _pseudoAxeEngines.clear();
  // Bouml preserved body end 00037A02
}


} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
