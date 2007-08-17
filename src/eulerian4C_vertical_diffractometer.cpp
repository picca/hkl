
#include "eulerian4C_vertical_diffractometer.h"

namespace hkl {

namespace eulerian4C {

namespace vertical {

Diffractometer::Diffractometer() :
 hkl::DiffractometerTemp<hkl::eulerian4C::vertical::Geometry>("Eulerian 4C Generic Soleil",
"This diffractometer was design by Frédéric-emmanuel PICCA\n\
* modes: bissector, delta theta, constant omega, constant chi, constant phi.\n\
* pseudoAxes: Psi.")
{
  // Bouml preserved body begin 00037A82
      // On met Ã  jour la liste des modes utilisables.
      _modes.add( new hkl::eulerian4C::vertical::mode::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", *_geom_T) );
      _modes.add( new hkl::eulerian4C::vertical::mode::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", *_geom_T) );
      _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Omega("Constant Omega", "Omega = Constante.", *_geom_T) );
      _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Chi("Constant Chi", "chi = Constante.", *_geom_T) );
      _modes.add( new hkl::eulerian4C::vertical::mode::Constant_Phi("Constant Phi", "phi = Constante.", *_geom_T) );
      
      // On ajoute les pseudoAxes
      _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Th2th(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Q2th(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Q(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian4C::vertical::pseudoAxeEngine::Psi(*_geom_T, _samples) );
  // Bouml preserved body end 00037A82
}

Diffractometer::~Diffractometer() 
{
  // Bouml preserved body begin 00037B02
      // On supprime les modes.
      _modes.clear();
      // On supprime les pseudoAxes.
      _pseudoAxeEngines.clear();
  // Bouml preserved body end 00037B02
}


} // namespace hkl::eulerian4C::vertical

} // namespace hkl::eulerian4C

} // namespace hkl
