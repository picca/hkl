#include "diffractometer_eulerian6C.h"
#include "mode_eulerian6C.h"
#include "pseudoaxe_eulerian6C.h"

namespace hkl {
    namespace diffractometer {

        // Default constructor.
        Eulerian6C::Eulerian6C() : Diffractometer<geometry::Eulerian6C>()
        {
          set_name("Eulerian 6C Generic Soleil");

          // On met à jour la liste des modes utilisables.
          m_modeList.add( new mode::eulerian6C::eulerian4C::vertical::Bissector);
          m_modeList.add( new mode::eulerian6C::eulerian4C::vertical::Delta_Theta);
          m_modeList.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Omega);
          m_modeList.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Chi);
          m_modeList.add( new mode::eulerian6C::eulerian4C::vertical::Constant_Phi);

          // On met à jour les pseudo moteurs
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::Tth(m_geometry) );
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::Q(m_geometry) );
          m_pseudoAxeList.add( new pseudoAxe::eulerian6C::eulerian4C::vertical::Psi(m_geometry) );
        }

        // Destructor.
        Eulerian6C::~Eulerian6C()
          {
            m_modeList.free();
            m_pseudoAxeList.free();
          }

    } // namespace diffractometer
} // namespace hkl
