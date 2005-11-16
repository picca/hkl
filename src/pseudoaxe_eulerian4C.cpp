#include "pseudoaxe_eulerian4C.h"

namespace hkl {
  namespace pseudoAxe {

    Eulerian4C::Eulerian4C(void) {}

    Eulerian4C::~Eulerian4C(void) {}

    namespace eulerian4C 
    {
      /*****************/
      /* PSI PSEUDOAXE */
      /*****************/
      Psi::Psi(void)
      {
        set_name("Psi");
        set_description("Psi is the angle of rotation around the Q vector.");
      }

      Psi::~Psi(void) {}

      void
      Psi::init(Geometry const & geometry)
      {
        m_geometry_E4C = geometry;
        m_Q = geometry.getQ();
      }
      
      double const
      Psi::get_value(Geometry const & geometry) const
      {
        double value;
        svector psi_axe;
        
        Quaternion qpsi = geometry.getSampleQuaternion();
        Quaternion qpsi0 = m_geometry_E4C.getSampleQuaternion().conjugate(); 
        qpsi *= qpsi0;

        qpsi.getAngleAndAxe(value, psi_axe);

       
        if (psi_axe == m_Q)
          return value;
        else
          return value;
      }

      void
      Psi::set_value(Geometry & geometry, double value) throw (HKLException)
      {
        Quaternion qm0 = m_geometry_E4C.getSampleQuaternion();

        Quaternion q(value, m_Q);
        
        std::cout << m_Q;
        
        std::cout << q.asMatrix();

        q *= qm0;

        smatrix M = q.asMatrix();

        std::cout << M;

        double omega = atan2(-M.get(0, 1), M.get(2, 1));
        double chi = atan2(sqrt(M.get(0, 1)*M.get(0, 1)+M.get(2, 1)*M.get(2, 1)), M.get(1,1));
        double phi = atan2(M.get(1, 0), M.get(1, 2));
        
        geometry.get_axe("omega").set_value(omega);
        geometry.get_axe("chi").set_value(chi);
        geometry.get_axe("phi").set_value(phi);
      }

    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl
