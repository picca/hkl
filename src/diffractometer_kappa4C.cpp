#include "diffractometer_kappa4C.h"
#include "geometry_kappa4C.h"

namespace hkl {
    namespace diffractometer {

        Kappa4C::Kappa4C(double alpha) : Kappa(alpha)
          {
            // On met à jour le nom.
            set_name("Kappa 4 Circles Generic Soleil");

            set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                            * modes: .\n\
                            * pseudoAxes: .");

            // On s'occupe de définir les axes de rotation du diffractomètre.
            m_geometry = new geometry::Kappa4C(getParameterValue("alpha"));

            // On ajouta les modes.
            m_modeList.add(new mode::Dummy);
          }

        Kappa4C::~Kappa4C(void)
          {
            delete m_geometry;

            // On supprime les modes.
            ModeList::iterator iter_mode = m_modeList.begin();
            ModeList::iterator last_mode = m_modeList.end();
            while(iter_mode != last_mode)
              {
                delete iter_mode->second;
                ++iter_mode;
              }
          }

    } // namespace diffractometer
} // namespace hkl
