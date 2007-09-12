
#include "kappa4C_vertical_diffractometer.h"

namespace hkl
  {

  namespace kappa4C
    {

    namespace vertical
      {

      Diffractometer::Diffractometer(double alpha) :
          hkl::DiffractometerTemp<hkl::kappa4C::vertical::Geometry>("Vertical Kappa 4 Circles Generic Soleil",
                                                                    "This diffractometer was design by Frédéric-emmanuel PICCA\n\
                                                                    * modes: .\n\
                                                                    * pseudoAxes: .", alpha)
      {
        // On ajouta les modes.
        _modes.add( new hkl::kappa4C::vertical::mode::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", *_geom_T) );
        _modes.add( new hkl::kappa4C::vertical::mode::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", *_geom_T) );
        _modes.add( new hkl::kappa4C::vertical::mode::Constant_Omega("Constant Omega", "Omega = Constante.", *_geom_T) );
        _modes.add( new hkl::kappa4C::vertical::mode::Constant_Chi("Constant Chi", "chi = Constante.", *_geom_T) );
        _modes.add( new hkl::kappa4C::vertical::mode::Constant_Phi("Constant Phi", "phi = Constante.", *_geom_T) );

        _pseudoAxeEngines.push_back( new hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::kappa4C::vertical::pseudoAxeEngine::Th2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::kappa4C::vertical::pseudoAxeEngine::Q2th(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::kappa4C::vertical::pseudoAxeEngine::Q(*_geom_T) );
        _pseudoAxeEngines.push_back( new hkl::kappa4C::vertical::pseudoAxeEngine::Psi(*_geom_T, _samples) );
      }

      Diffractometer::~Diffractometer()
      {
        // On supprime les modes.
        _modes.clear();

        // On supprime les pseudoAxes.
        _pseudoAxeEngines.clear();
      }


    } // namespace hkl::kappa4C::vertical

  } // namespace hkl::kappa4C

} // namespace hkl
