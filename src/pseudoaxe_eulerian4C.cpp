#include "pseudoaxe_eulerian4C.h"

namespace hkl {
  namespace pseudoAxe {

    Eulerian4C::Eulerian4C(void)
      : PseudoAxe()
    {}

    Eulerian4C::~Eulerian4C(void) {}

    namespace eulerian4C 
    {
      /*****************/
      /* PSI PSEUDOAXE */
      /*****************/
      Psi::Psi(void)
        : Eulerian4C()
      {
        set_name("psi");
        set_description("psi is the angle of rotation around the Q vector."); 
      }

      Psi::Psi(Psi const & psi)
        : Eulerian4C(psi)
      {}
        
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
    
        cout << "value : " << value * constant::math::radToDeg << endl;
        
        if (psi_axe == m_Q)
          return value;
        else
          return value;
      }

      void
      Psi::set_value(Geometry & geometry, double value) throw (HKLException)
      {
        cout << "value : " << value * constant::math::radToDeg << endl;
        cout << geometry.get_source();
        cout << geometry;
        cout << "Q: " << geometry.getQ() << endl;
        
        Quaternion qm0 = m_geometry_E4C.getSampleQuaternion();

        cout << "qm0: " << qm0 << endl;
        svector axe;
        double angle;
        qm0.getAngleAndAxe(angle, axe);
        cout << "angle and axe: " << angle*constant::math::radToDeg << " " << axe << endl;
        
        Quaternion q(value, m_Q);
        
        cout << "q: " << q << endl;

        q *= qm0;

        cout << "q * qm0: " << q << endl;

        q.getAngleAndAxe(angle, axe);
        cout << "angle and axe: " << angle*constant::math::radToDeg << " " << axe << endl;
        
        smatrix M = q.asMatrix();

        cout << "M: " << M;
        
        double omega;
        double chi;
        double phi;
        if (fabs(M.get(0, 1)) < constant::math::epsilon_0 
          && fabs(M.get(1, 0)) < constant::math::epsilon_0
          && fabs(M.get(2, 1)) < constant::math::epsilon_0
          && fabs(M.get(1, 2)) < constant::math::epsilon_0)
        {
          omega = atan2(M.get(2, 0), M.get(0, 0));
          chi = 0.;
          phi = 0.;
        } else {
          omega = Mode::_atan2(-M.get(0, 1), M.get(2, 1));
          chi = Mode::_atan2(sqrt(M.get(0, 1)*M.get(0, 1)+M.get(2, 1)*M.get(2, 1)), M.get(1,1));
          phi = Mode::_atan2(-M.get(1, 0), -M.get(1, 2));
        }
        
        geometry.get_axe("omega").set_value(omega);
        geometry.get_axe("chi").set_value(chi);
        geometry.get_axe("phi").set_value(phi);
 
        cout << geometry;
        cout << "Q: " << geometry.getQ() << endl;
      }

    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl
