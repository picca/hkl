#include "diffractometer_eulerian6C.h"
#include "mode_eulerian6C.h"
#include "pseudoaxe_eulerian6C.h"

namespace hkl
  {
  namespace diffractometer
    {

// Default constructor.
    Eulerian6C::Eulerian6C() :
        DiffractometerTemp<geometry::Eulerian6C>("Eulerian 6C Generic Soleil", "Soleil")
    {
      // On met à jour la liste des modes utilisables.
      _modes.add( new mode::eulerian6C::eulerian4C::vertical::Bissector("Bissector", "Bissector", _geom_T));
      _modes.add( new mode::eulerian6C::eulerian4C::vertical::Delta_Theta("Delta Theta", "Delta Theta", _geom_T));
      _modes.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Omega("Constant Omega", "Constant Omega", _geom_T));
      _modes.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Chi("Constant Chi", "Constant Chi", _geom_T));
      _modes.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Phi("Constant Phi", "Constant Phi", _geom_T));

      // On met à jour les pseudo moteurs
      _pseudoAxes.add( new pseudoAxe::eulerian6C::Tth(_geom_T) );
      _pseudoAxes.add( new pseudoAxe::eulerian6C::Q(_geom_T) );
      _pseudoAxes.add( new pseudoAxe::eulerian6C::eulerian4C::vertical::Psi(_geom_T, "psi", "psi") );
    }

// Destructor.
    Eulerian6C::~Eulerian6C()
    {
      _modes.clear();
      _pseudoAxes.clear();
    }

  } // namespace diffractometer
} // namespace hkl
