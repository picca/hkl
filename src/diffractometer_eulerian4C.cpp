#include "diffractometer_eulerian4C.h"
#include "pseudoaxe_eulerian4C.h"
#include "mode_eulerian4C.h"

namespace hkl
  {
  namespace diffractometer
    {
    namespace eulerian4C
      {

      Vertical::Vertical(void) :
          DiffractometerTemp<geometry::eulerian4C::Vertical>("Eulerian 4C Generic Soleil",
              "This diffractometer was design by Frédéric-emmanuel PICCA\n\
              * modes: bissector, delta theta, constant omega, constant chi, constant phi.\n\
              * pseudoAxes: Psi.")
      {
        // On met à jour la liste des modes utilisables.
        _modes.add(new mode::eulerian4C::vertical::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", _geom_T));
        _modes.add(new mode::eulerian4C::vertical::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", _geom_T));
        _modes.add(new mode::eulerian4C::vertical::Constant_Omega("Constant Omega", "Omega = Constante.", _geom_T));
        _modes.add(new mode::eulerian4C::vertical::Constant_Chi("Constant Chi", "chi = Constante.", _geom_T));
        _modes.add(new mode::eulerian4C::vertical::Constant_Phi("Constant Phi", "phi = Constante.", _geom_T));

        // On ajoute les pseudoAxes
        _pseudoAxes.add(new pseudoAxe::eulerian4C::vertical::Psi(_geom_T));
        _pseudoAxes.add(new pseudoAxe::eulerian4C::vertical::twoC::Q(_geom_T, "q", "q"));
        _pseudoAxes.add(new pseudoAxe::eulerian4C::vertical::twoC::Th2th(_geom_T, "th2th", "th2th"));
        _pseudoAxes.add(new pseudoAxe::eulerian4C::vertical::twoC::Q2th(_geom_T, "q2th", "q2th"));
      }

      Vertical::~Vertical(void)
      {
        // On supprime les modes.
        _modes.clear();
        // On supprime les pseudoAxes.
        _pseudoAxes.clear();
      }

    } // namespace eulerian4C
  } // namespace diffractometer
} // namespace hkl
