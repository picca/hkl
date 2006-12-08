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
          Diffractometer<geometry::eulerian4C::Vertical>()
      {
        // On met à jour le nom.
        set_name("Eulerian 4C Generic Soleil");

        set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                        * modes: bissector, delta theta, constant omega, constant chi, constant phi.\n\
                        * pseudoAxes: Psi.");

        // On met à jour la liste des modes utilisables.
        m_modeList.add(new mode::eulerian4C::vertical::Bissector("Bissector", "Omega = 2theta / 2. \n there is no parameters for this mode.", _geometry));
        m_modeList.add(new mode::eulerian4C::vertical::Delta_Theta("Delta Theta", "Omega = theta + dtheta.", _geometry));
        m_modeList.add(new mode::eulerian4C::vertical::Constant_Omega("Constant Omega", "Omega = Constante.", _geometry));
        m_modeList.add(new mode::eulerian4C::vertical::Constant_Chi("Constant Chi", "chi = Constante.", _geometry));
        m_modeList.add(new mode::eulerian4C::vertical::Constant_Phi("Constant Phi", "phi = Constante.", _geometry));

        // On ajoute les pseudoAxes
        m_pseudoAxeList.add(new pseudoAxe::eulerian4C::vertical::Psi(m_geometry));
        m_pseudoAxeList.add(new pseudoAxe::eulerian4C::vertical::twoC::Q(m_geometry));
        m_pseudoAxeList.add(new pseudoAxe::eulerian4C::vertical::twoC::Th2th(m_geometry));
        m_pseudoAxeList.add(new pseudoAxe::eulerian4C::vertical::twoC::Q2th(m_geometry));
      }

      Vertical::~Vertical(void)
      {
        // On supprime les modes.
        m_modeList.free();
        // On supprime les pseudoAxes.
        m_pseudoAxeList.free();
      }

    } // namespace eulerian4C
  } // namespace diffractometer
} // namespace hkl
