#include "diffractometer_kappa4C.h"
#include "pseudoaxe_kappa4C.h"
#include "mode_kappa4C.h"

namespace hkl
  {
  namespace diffractometer
    {
    namespace kappa4C
      {

      Vertical::Vertical(void) :
          DiffractometerTemp<geometry::kappa4C::Vertical>("Vertical Kappa 4 Circles Generic Soleil",
              "This diffractometer was design by Frédéric-emmanuel PICCA\n\
              * modes: .\n\
              * pseudoAxes: .")
      {
        // On ajouta les modes.
        _modes.add(new mode::kappa4C::vertical::eulerian4C::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", _geom_T));
        _modes.add(new mode::kappa4C::vertical::eulerian4C::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", _geom_T));
        _modes.add(new mode::kappa4C::vertical::eulerian4C::Constant_Omega("Constant Omega", "Omega = Constante.", _geom_T));
        _modes.add(new mode::kappa4C::vertical::eulerian4C::Constant_Chi("Constant Chi", "chi = Constante.", _geom_T));
        _modes.add(new mode::kappa4C::vertical::eulerian4C::Constant_Phi("Constant Phi", "phi = Constante.", _geom_T));

        // On ajoute les pseudoAxes
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::Omega(_geom_T));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::Chi(_geom_T));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::Phi(_geom_T));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::twoC::Th2th(_geom_T, "th2th", "th2th"));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::twoC::Q2th(_geom_T, "q2th", "q2th"));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::twoC::Q(_geom_T, "q", "q"));
        _pseudoAxes.add(new pseudoAxe::kappa4C::vertical::eulerian4C::Psi(_geom_T, "psi", "psi"));
      }

      Vertical::~Vertical(void)
      {
        // On supprime les modes.
        _modes.clear();

        // On supprime les pseudoAxes.
        _pseudoAxes.clear();
      }

    } // namespace kappa4C
  } // namespace diffractometer
} // namespace hkl
