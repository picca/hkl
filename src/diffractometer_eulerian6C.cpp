#include "diffractometer_eulerian6C.h"
#include "mode_eulerian6C.h"
#include "pseudoaxeengine_eulerian6C.h"

namespace hkl
  {
  namespace diffractometer
    {

// Default constructor.
    Eulerian6C::Eulerian6C() :
        DiffractometerTemp<geometry::Eulerian6C>("Eulerian 6C Generic Soleil", "Soleil")
    {
      // On met à jour la liste des modes utilisables.
      _modes.add( new mode::eulerian6C::Bissector("Bissector", "Bissector", _geom_T) );
      _modes.add( new mode::eulerian6C::Delta_Theta("Delta Theta", "Delta Theta", _geom_T) );
      _modes.add( new mode::eulerian6C::Constant_Omega("Constant Omega", "Constant Omega", _geom_T) );
      _modes.add( new mode::eulerian6C::Constant_Chi("Constant Chi", "Constant Chi", _geom_T) );
      _modes.add( new mode::eulerian6C::Constant_Phi("Constant Phi", "Constant Phi", _geom_T) );

      // On met à jour les pseudo moteurs
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::eulerian6C::Tth(_geom_T) );
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::eulerian6C::Q(_geom_T) );
      _pseudoAxeEngines.push_back( new pseudoAxeEngine::eulerian6C::Psi(_geom_T) );
    }

// Destructor.
    Eulerian6C::~Eulerian6C()
    {
      _modes.clear();
      _pseudoAxeEngines.clear();
    }

  } // namespace diffractometer
} // namespace hkl
