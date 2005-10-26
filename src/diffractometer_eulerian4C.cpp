#include "diffractometer_eulerian4C.h"

namespace hkl {
  namespace diffractometer {

    Eulerian4C::Eulerian4C(void) : Diffractometer()
    {
      // On met à jour le nom.
      m_name = "Eulerian 4C Generic Soleil";

      // On s'occupe de définir les axes de rotation du diffractomètre.
      m_geometry = new geometry::Eulerian4C();

      // On met à jour la liste des modes utilisables.
      m_modeList.add( new mode::eulerian4C::Bissector());
      m_modeList.add( new mode::eulerian4C::Delta_Theta());
      m_modeList.add( new mode::eulerian4C::Constant_Omega());
      m_modeList.add( new mode::eulerian4C::Constant_Chi());
      m_modeList.add( new mode::eulerian4C::Constant_Phi());
    }

    Eulerian4C::~Eulerian4C(void)
    {
      delete m_geometry;

      // Ne pas oublier de supprimer les modes.
      ModeList::iterator iter = m_modeList.begin();
      ModeList::iterator last = m_modeList.end();

      while(iter != last){
#ifdef VCPP6
        delete iter->second;
#else
        delete *iter;
#endif
        ++iter;
      }
    }

  } // namespace diffractometer
} // namespace hkl
