
#include "eulerian6C_diffractometer.h"

namespace hkl
  {

  namespace eulerian6C
    {

    Diffractometer::Diffractometer() :
        DiffractometerTemp<hkl::eulerian6C::Geometry>("Eulerian 6C Generic Soleil", "Soleil")
    {
      // On met à jour la liste des modes utilisables.
      _modes.add( new hkl::eulerian6C::mode::Bissector("Bissector", "Bissector", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Delta_Theta("Delta Theta", "Delta Theta", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Omega("Constant Omega", "Constant Omega", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Chi("Constant Chi", "Constant Chi", *_geom_T) );
      _modes.add( new hkl::eulerian6C::mode::Constant_Phi("Constant Phi", "Constant Phi", *_geom_T) );

      // On met à jour les pseudo moteurs
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Tth(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Q(*_geom_T) );
      _pseudoAxeEngines.push_back( new hkl::eulerian6C::pseudoAxeEngine::Psi(*_geom_T, _samples) );
    }

    Diffractometer::~Diffractometer()
    {
      _modes.clear();
      _pseudoAxeEngines.clear();
    }


  } // namespace hkl::eulerian6C

} // namespace hkl
