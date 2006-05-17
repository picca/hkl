#include "diffractometer_twoC.h"
#include "pseudoaxe_twoC.h"
#include "geometry_twoC.h"
#include "mode_twoC.h"

namespace hkl {
    namespace diffractometer {
        namespace twoC {

            Vertical::Vertical(void) : Diffractometer()
            {
              // On met à jour le nom.
              set_name("2C Generic Soleil");

              set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                              * modes: \"Symetric\", \"Fix incidence\"\n\
                              * pseudoAxes: \"th2th\", \"q2th\", \"q\"");

              // On s'occupe de définir les axes de rotation du diffractomètre.
              m_geometry = new geometry::twoC::Vertical;

              // On met à jour la liste des modes utilisables.
              m_modeList.add(new mode::twoC::vertical::Symetric);
              m_modeList.add(new mode::twoC::vertical::Fix_Incidence);

              // On ajoute les pseudoAxes
              m_pseudoAxeList.add(new pseudoAxe::twoC::vertical::Th2th);
              m_pseudoAxeList.add(new pseudoAxe::twoC::vertical::Q2th);
              m_pseudoAxeList.add(new pseudoAxe::twoC::vertical::Q);
            }

            Vertical::~Vertical(void)
              {
                // On supprime la geometrie.
                delete m_geometry;
                // On supprime les modes.
                m_modeList.free();
                // On supprime les pseudoAxes.
                m_pseudoAxeList.free();
              }

        } //namespace twoC
    } // namespace diffractometer
} // namespace hkl
