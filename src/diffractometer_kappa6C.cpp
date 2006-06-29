#include "diffractometer_kappa6C.h"
#include "pseudoaxe_kappa6C.h"
#include "mode_kappa6C.h"

namespace hkl {
    namespace diffractometer {

        Kappa6C::Kappa6C(void) : Diffractometer<geometry::Kappa6C>()
        {
          // On met à jour le nom.
          set_name("Kappa 6 Circles Generic Soleil");

          set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                          * modes: .\n\
                          * pseudoAxes: .");

          // On ajoute les modes.
          m_modeList.add(new mode::kappa6C::eulerian4C::vertical::Bissector);
          m_modeList.add(new mode::kappa6C::eulerian4C::vertical::Delta_Theta);
          m_modeList.add(new mode::kappa6C::eulerian4C::vertical::Constant_Omega);
          m_modeList.add(new mode::kappa6C::eulerian4C::vertical::Constant_Chi);
          m_modeList.add(new mode::kappa6C::eulerian4C::vertical::Constant_Phi);

          // On ajoute les pseudo axes.
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::kappa4C::vertical::Omega);
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::kappa4C::vertical::Chi);
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::kappa4C::vertical::Phi);
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::eulerian4C::vertical::Psi);
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::eulerian6C::Tth);
          m_pseudoAxeList.add(new pseudoAxe::kappa6C::eulerian6C::Q);
        }

        Kappa6C::~Kappa6C(void)
          {
            // On supprime les modes.
            m_modeList.free();

            // On supprime les pseudoAxes.
            m_pseudoAxeList.free();
          }

    } // namespace diffractometer
} // namespace hkl
