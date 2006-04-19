#include "diffractometer_kappa6C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace diffractometer {

        Kappa6C::Kappa6C(double alpha) : Kappa(alpha)
          {
            // On met à jour le nom.
            set_name("Kappa 6 Circles Generic Soleil");

            set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                            * modes: .\n\
                            * pseudoAxes: .");

            // On s'occupe de définir les axes de rotation du diffractomètre.
            m_geometry = new geometry::Kappa6C(getParameterValue("alpha"));

            // On ajoute les modes.
            m_modeList.add(new mode::Dummy);
          }

        Kappa6C::~Kappa6C(void)
          {
            delete m_geometry;
 
            // On supprime les modes.
            m_modeList.free();
          }

    } // namespace diffractometer
} // namespace hkl
