
#include "kappa6C_diffractometer.h"

namespace hkl {

namespace kappa6C {

Diffractometer::Diffractometer(double alpha) :
  hkl::DiffractometerTemp<hkl::kappa6C::Geometry>("Kappa 6 Circles Generic Soleil", "This diffractometer was design by Frédéric-emmanuel PICCA\n\
                                              * modes: .\n\
                                              * pseudoAxes: .", alpha)
{
  // Bouml preserved body begin 00037D82
  // On ajoute les modes.
  _modes.add( new hkl::kappa6C::mode::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", *_geom_T) );
  _modes.add( new hkl::kappa6C::mode::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", *_geom_T) );
  _modes.add( new hkl::kappa6C::mode::Constant_Omega("Constant Omega", "Omega = Constante.", *_geom_T) );
  _modes.add( new hkl::kappa6C::mode::Constant_Chi("Constant Chi", "chi = Constante.", *_geom_T) );
  _modes.add( new hkl::kappa6C::mode::Constant_Phi("Constant Phi", "phi = Constante.", *_geom_T) );

  // On ajoute les pseudo axes.
  _pseudoAxeEngines.push_back( new hkl::kappa6C::pseudoAxeEngine::Eulerians(*_geom_T, alpha) );
  _pseudoAxeEngines.push_back( new hkl::kappa6C::pseudoAxeEngine::Psi(*_geom_T, _samples) );
  _pseudoAxeEngines.push_back( new hkl::kappa6C::pseudoAxeEngine::Tth(*_geom_T) );
  _pseudoAxeEngines.push_back( new hkl::kappa6C::pseudoAxeEngine::Q(*_geom_T) );
  // Bouml preserved body end 00037D82
}

Diffractometer::~Diffractometer() 
{
  // Bouml preserved body begin 00037E02
      // On supprime les modes.
      _modes.clear();
      
      // On supprime les pseudoAxes.
      _pseudoAxeEngines.clear();
  // Bouml preserved body end 00037E02
}


} // namespace hkl::kappa6C

} // namespace hkl
