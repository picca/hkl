#include "diffractometer_kappa4C_wrap.h"
#include "pseudoaxe_kappa4C.h"
#include "geometry_kappa4C.h"
#include "mode_kappa4C.h"
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
    m_geometry = new geometry::kappa4C::Vertical(getParameterValue("alpha"));
   
    // On ajouta les modes.
    m_modeList.add(new mode::kappa4C::vertical::Bissector);
    m_modeList.add(new mode::kappa4C::vertical::Delta_Theta);
    m_modeList.add(new mode::kappa4C::vertical::Constant_Omega);
    m_modeList.add(new mode::kappa4C::vertical::Constant_Chi);
    m_modeList.add(new mode::kappa4C::vertical::Constant_Phi);

    // On ajoute les pseudoAxes
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::vertical::Omega(alpha * constant::math::degToRad));
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::vertical::Chi(alpha * constant::math::degToRad));
    m_pseudoAxeList.add(new pseudoAxe::kappa4C::vertical::Phi(alpha * constant::math::degToRad));
}

Diffractometer_Kappa4C_wrap::~Diffractometer_Kappa4C_wrap(void)
{
    // On supprime la geometrie.
    delete m_geometry;

    // On supprime les modes.
    m_modeList.free();

    // On supprime les pseudoAxes.
    m_pseudoAxeList.free();
}
