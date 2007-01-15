#include "diffractometer_kappa6C.h"
#include "pseudoaxe_kappa6C.h"
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
      _modes.add(new mode::kappa6C::eulerian4C::vertical::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", _geom_T));
      _modes.add(new mode::kappa6C::eulerian4C::vertical::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", _geom_T));
      _modes.add(new mode::kappa6C::eulerian4C::vertical::Constant_Omega("Constant Omega", "Omega = Constante.", _geom_T));
      _modes.add(new mode::kappa6C::eulerian4C::vertical::Constant_Chi("Constant Chi", "chi = Constante.", _geom_T));
      _modes.add(new mode::kappa6C::eulerian4C::vertical::Constant_Phi("Constant Phi", "phi = Constante.", _geom_T));

      // On ajoute les pseudo axes.
      vector<string> names;
      _pseudoAxeEngine = new pseudoAxeEngine::kappa6C::kappa4C::vertical::Eulerians(_geom_T, names);

      _pseudoAxes.add(new pseudoAxe::kappa6C::kappa4C::vertical::Omega(_geom_T, "omega", "omega"));
      _pseudoAxes.add(new pseudoAxe::kappa6C::kappa4C::vertical::Chi(_geom_T, "chi", "chi"));
      _pseudoAxes.add(new pseudoAxe::kappa6C::kappa4C::vertical::Phi(_geom_T, "phi", "phi"));
      _pseudoAxes.add(new pseudoAxe::kappa6C::eulerian4C::vertical::Psi(_geom_T, "psi", "psi"));
      _pseudoAxes.add(new pseudoAxe::kappa6C::eulerian6C::Tth(_geom_T, "tth", "tth"));
      _pseudoAxes.add(new pseudoAxe::kappa6C::eulerian6C::Q(_geom_T, "q", "q"));
    }

    Kappa6C::~Kappa6C(void)
    {
      // On supprime les modes.
      _modes.clear();

      // On supprime les pseudoAxes.
      _pseudoAxes.clear();

      delete _pseudoAxeEngine;
    }

  } // namespace diffractometer
} // namespace hkl
