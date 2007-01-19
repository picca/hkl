#include "diffractometer_twoC.h"
#include "pseudoaxeengine_twoC.h"
#include "mode_twoC.h"

namespace hkl
  {
  namespace diffractometer
    {
    namespace twoC
      {

      Vertical::Vertical(void) :
          DiffractometerTemp<geometry::twoC::Vertical>("2C Generic Soleil", "This diffractometer was design by Frédéric-emmanuel PICCA\n\
              * modes: \"Symetric\", \"Fix incidence\"\n\
              * pseudoAxes: \"th2th\", \"q2th\", \"q\"")
      {
        // On met à jour la liste des modes utilisables.
        _modes.add( new mode::twoC::vertical::Symetric("Symetric", "Omega = 2theta / 2. = theta", _geom_T) );
        _modes.add( new mode::twoC::vertical::Fix_Incidence("Fix incidence", "2theta = 2 * theta, omega is free.", _geom_T) );

        // On ajoute les pseudoAxes
        _pseudoAxeEngines.add( new pseudoAxeEngine::twoC::vertical::Th2th(_geom_T) );
        _pseudoAxeEngines.add( new pseudoAxeEngine::twoC::vertical::Q2th(_geom_T) );
        _pseudoAxeEngines.add( new pseudoAxeEngine::twoC::vertical::Q(_geom_T) );
      }

      Vertical::~Vertical(void)
      {
        // On supprime les modes.
        _modes.clear();
        // On supprime les pseudoAxes.
        _pseudoAxeEngines.clear();
      }

    } //namespace twoC
  } // namespace diffractometer
} // namespace hkl
