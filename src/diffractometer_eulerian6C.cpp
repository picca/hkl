#include "diffractometer_eulerian6C.h"

namespace hkl {
    namespace diffractometer {

        // Default constructor.
        Eulerian6C::Eulerian6C() : Diffractometer()
        {
          set_name("Eulerian 6C Generic Soleil");

          m_geometry = new geometry::Eulerian6C();

          // On met à jour la liste des modes utilisables.
          m_modeList.add( new mode::eulerian6C::horizontal4C::Bissector());
          m_modeList.add( new mode::eulerian6C::horizontal4C::Delta_Theta());
          m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Omega());
          m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Chi());
          m_modeList.add( new mode::eulerian6C::horizontal4C::Constant_Phi());
          m_modeList.add( new mode::eulerian6C::vertical4C::Bissector());
          m_modeList.add( new mode::eulerian6C::vertical4C::Delta_Theta());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Omega());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Chi());
          m_modeList.add( new mode::eulerian6C::vertical4C::Constant_Phi());
        }

        // Destructor.
        Eulerian6C::~Eulerian6C()
          {
            delete m_geometry;

            m_modeList.free();
          }

    } // namespace diffractometer
} // namespace hkl
