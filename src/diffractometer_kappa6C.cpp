#include "diffractometer_kappa6C.h"
#include "pseudoaxeengine_kappa6C.h"
#include "mode_kappa6C.h"

namespace hkl
  {
  namespace diffractometer
    {

    Kappa6C::Kappa6C(double alpha) :
        DiffractometerTemp<geometry::Kappa6C>("Kappa 6 Circles Generic Soleil", "This diffractometer was design by Frédéric-emmanuel PICCA\n\
                                              * modes: .\n\
                                              * pseudoAxes: .")
    {
      // On ajoute les modes.
      _modes.add( new mode::kappa6C::eulerian4C::vertical::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", _geom_T) );
      _modes.add( new mode::kappa6C::eulerian4C::vertical::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", _geom_T) );
      _modes.add( new mode::kappa6C::eulerian4C::vertical::Constant_Omega("Constant Omega", "Omega = Constante.", _geom_T) );
      _modes.add( new mode::kappa6C::eulerian4C::vertical::Constant_Chi("Constant Chi", "chi = Constante.", _geom_T) );
      _modes.add( new mode::kappa6C::eulerian4C::vertical::Constant_Phi("Constant Phi", "phi = Constante.", _geom_T) );

      // On ajoute les pseudo axes.
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::kappa6C::Psi(_geom_T) );
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::kappa6C::Eulerians(_geom_T) );
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::kappa6C::Tth(_geom_T) );
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::kappa6C::Q(_geom_T) );
    }

    Kappa6C::~Kappa6C(void)
    {
      // On supprime les modes.
      _modes.clear();

      // On supprime les pseudoAxes.
      _pseudoAxeEngines.clear();
    }

  } // namespace diffractometer
} // namespace hkl
