#include "pseudoaxe_eulerian4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {

        Eulerian4C::Eulerian4C(void) :
          PseudoAxe()
        {}

        Eulerian4C::~Eulerian4C(void)
          {}

        ostream &
        Eulerian4C::toStream(ostream & flux) const
          {
            PseudoAxe::toStream (flux);
            m_geometry_E4C.toStream (flux);

            return flux;
          }

        istream &
        Eulerian4C::fromStream(istream & flux)
          {
            PseudoAxe::fromStream (flux);
            m_geometry_E4C.fromStream (flux);

            return flux;
          }

        namespace eulerian4C {
            /*****************/
            /* PSI PSEUDOAXE */
            /*****************/
            Psi::Psi(void) :
              Eulerian4C ()
            {
              set_name ("psi");
              set_description ("psi is the angle of rotation around the Q vector.");
            }

            Psi::~Psi(void)
              {}

            void Psi::init(Geometry const & geometry)
              {
                m_geometry_E4C = geometry;
                m_Q = geometry.getQ();
                m_Q /= m_Q.norm2();
                set_wasInitialized(true);
              }

            double const 
            Psi::get_value(Geometry const & geometry)
              {
                double value;
                svector psi_axe(m_Q);

                Quaternion qpsi = geometry.getSampleQuaternion();
                Quaternion qpsi0 = m_geometry_E4C.getSampleQuaternion().conjugate();
                qpsi *= qpsi0;

                qpsi.getAngleAndAxe(value, psi_axe);

                //cout << " obtained : " << value * constant::math::radToDeg << " " << psi_axe << " " << m_Q << endl;

                if (psi_axe == m_Q)
                    set_isValid(true);
                else
                    set_isValid(false);
                return value;
              }

            void
            Psi::set_value(Geometry & geometry,
                           double value) throw (HKLException)
              {
                //cout << "asked value: " << value * constant::math::radToDeg;
                Quaternion qm0 = m_geometry_E4C.getSampleQuaternion();

                Quaternion q(value, m_Q);

                q *= qm0;

                smatrix M = q.asMatrix();

                double omega;
                double chi;
                double phi;
                double two_theta;
                if (fabs (M.get(0, 1)) < constant::math::epsilon_0
                    && fabs (M.get(1, 0)) < constant::math::epsilon_0
                    && fabs (M.get(2, 1)) < constant::math::epsilon_0
                    && fabs (M.get(1, 2)) < constant::math::epsilon_0)
                  {
                    omega = geometry.get_axe("omega").get_value();
                    if (M.get (1, 1) > 0)
                      {
                        chi = 0;
                        phi = atan2(M.get(2, 0), M.get(0, 0)) - omega;
                      }
                    else
                      {
                        chi = constant::math::pi;
                        phi = omega - atan2(M.get(2, 0), M.get(0, 0));
                      }
                    geometry.get_axe("chi").set_value(chi);
                    geometry.get_axe("phi").set_value(phi);
                  }
                else
                  {
                    //1st solution 0<chi<pi
                    omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
                    chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                    phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
                    two_theta = geometry.get_axe("2theta").get_value();
                    geometry::Eulerian4C g1(omega, chi, phi, two_theta);

                    //2nd solution -pi<chi<0
                    omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
                    chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                    phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
                    geometry::Eulerian4C g2(omega, chi, phi, two_theta);

                    double d1 = geometry.getDistance(g1);
                    double d2 = geometry.getDistance(g2);
                    if (d1 < d2)
                      {
                        geometry.get_axe("omega").set_value(g1.get_axe("omega").get_value());
                        geometry.get_axe("chi").set_value(g1.get_axe("chi").get_value());
                        geometry.get_axe("phi").set_value(g1.get_axe("phi").get_value());
                      }
                    else
                      {
                        geometry.get_axe("omega").set_value(g2.get_axe("omega").get_value());
                        geometry.get_axe("chi").set_value(g2.get_axe("chi").get_value());
                        geometry.get_axe("phi").set_value(g2.get_axe("phi").get_value());
                      }
                  }
              }

            ostream &
            Psi::toStream(ostream & flux) const
              {
                Eulerian4C::toStream (flux);
                m_Q.toStream (flux);

                return flux;
              }

            istream &
            Psi::fromStream (istream & flux)
              {
                Eulerian4C::fromStream (flux);
                m_Q.fromStream (flux);

                return flux;
              }

        }				// namespace eulerian4C
    }				// namespace pseudoAxe
}				// namespace hkl
