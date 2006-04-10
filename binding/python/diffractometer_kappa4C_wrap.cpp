#include "diffractometer_kappa4C_wrap.h"
#include "pseudoaxe_kappa4C.h"
#include "geometry_kappa4C.h"
#include "constants.h"
#include "mode.h"

Diffractometer_Kappa4C_wrap::Diffractometer_Kappa4C_wrap(double alpha)
: Diffractometer_Kappa_wrap(alpha)
{
    // On met à jour le nom.
    set_name("Kappa 4 Circles Generic Soleil");

    set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                    * modes: .\n\
                    * pseudoAxes: .");

    // On s'occupe de définir les axes de rotation du diffractomètre.
    cout << getParameterValue("alpha") * constant::math::radToDeg << endl;
    m_geometry = new geometry::Kappa4C(getParameterValue("alpha"));
   
    // On ajouta les modes.
    m_modeList.add(new mode::Dummy);

    // On ajoute les pseudoAxes
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::Omega(alpha * constant::math::degToRad));
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::Chi(alpha * constant::math::degToRad));
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::Phi(alpha * constant::math::degToRad));
}

Diffractometer_Kappa4C_wrap::~Diffractometer_Kappa4C_wrap(void)
{
    // On supprime la geometrie.
    delete m_geometry;

    // On supprime les modes.
    ModeList::iterator iter_mode = m_modeList.begin();
    ModeList::iterator last_mode = m_modeList.end();
    while(iter_mode != last_mode)
      {
        delete iter_mode->second;
        ++iter_mode;
      }

    // On supprime les pseudoAxes.
    PseudoAxeList::iterator iter_pseudo = m_pseudoAxeList.begin();
    PseudoAxeList::iterator last_pseudo = m_pseudoAxeList.end();
    while(iter_pseudo != last_pseudo)
      {
        delete iter_pseudo->second;
        ++iter_pseudo;
      }
}
