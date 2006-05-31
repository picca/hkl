#include "diffractometer_eulerian6C.h"

namespace hkl {
    namespace diffractometer {

        // Default constructor.
        Eulerian6C::Eulerian6C() : Diffractometer()
        {
          set_name("Eulerian 6C Generic Soleil");

          m_geometry = new geometry::Eulerian6C();

          // On met à jour la liste des modes utilisables.
          m_modeList.add( new mode::eulerian6C::vertical4C::Bissector());
          m_modeList.add( new mode::eulerian6C::vertical4C::Delta_Theta());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Omega());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Chi());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Phi());

          // On met à jour les pseudo moteurs
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::Tth );
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::Q );
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::eulerian4C::vertical::Psi );
        }

        // Destructor.
        Eulerian6C::~Eulerian6C()
          {
            delete m_geometry;

            m_modeList.free();

            m_pseudoAxeList.free();
          }

    } // namespace diffractometer
} // namespace hkl
