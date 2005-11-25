#include "diffractometer_eulerian4C.h"

namespace hkl {
  namespace diffractometer {

    Eulerian4C::Eulerian4C(void) : Diffractometer()
    {
      // On met à jour le nom.
      set_name("Eulerian 4C Generic Soleil");

      set_description("This diffractometer was design by Frédéric-emmanuel PICCA\n\
                       * modes: bissector, delta theta, constant omega, constant chi, constant phi.\n\
                       * pseudoAxes: Psi.");

      // On s'occupe de définir les axes de rotation du diffractomètre.
      m_geometry = new geometry::Eulerian4C;
      
      // On met à jour la liste des modes utilisables.
      m_modeList.add(new mode::eulerian4C::Bissector);
      m_modeList.add(new mode::eulerian4C::Delta_Theta);
      m_modeList.add(new mode::eulerian4C::Constant_Omega);
      m_modeList.add(new mode::eulerian4C::Constant_Chi);
      m_modeList.add(new mode::eulerian4C::Constant_Phi);

      // On ajoute les pseudoAxes
      m_pseudoAxeList.add(new pseudoAxe::eulerian4C::Psi);
    }

    Eulerian4C::~Eulerian4C(void)
    {
      // On supprime la geometrie.
      delete m_geometry;

      // On supprime les modes.
      ModeList::iterator iter_mode = m_modeList.begin();
      ModeList::iterator last_mode = m_modeList.end();
      while(iter_mode != last_mode)
      {
#ifdef VCPP6
        delete iter_mode->second;
#else
        delete *iter_mode;
#endif
        ++iter_mode;
      }
      
      // On supprime les pseudoAxes.
      PseudoAxeList::iterator iter_pseudo = m_pseudoAxeList.begin();
      PseudoAxeList::iterator last_pseudo = m_pseudoAxeList.end();
      while(iter_pseudo != last_pseudo)
      {
        delete *iter_pseudo;
        ++iter_pseudo;
      }
    }

  } // namespace diffractometer
} // namespace hkl
