#include "pseudoaxe_eulerian4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian4C {

            Vertical::Vertical(void)
            : PseudoAxe()
              {}

            Vertical::~Vertical(void)
              {}

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                PseudoAxe::toStream(flux);
                m_geometry_E4C.toStream (flux);

                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                PseudoAxe::fromStream(flux);
                m_geometry_E4C.fromStream (flux);

                return flux;
              }

            namespace vertical {
                /*****************/
                /* PSI PSEUDOAXE */
                /*****************/
                Psi::Psi(void) :
                  Vertical()
                {
                  set_name ("psi");
                  set_description ("psi is the angle of rotation around the Q vector.");
                }

                Psi::~Psi(void)
                  {}

                void
                Psi::initialize(Geometry const & geometry)
                  {
                    m_geometry_E4C = geometry;
                    m_Q = geometry.getQ();
                    m_Q /= m_Q.norm2();
                    set_wasInitialized(true);
                  }

                bool
                Psi::get_isValid(Geometry const & geometry) const
                  {
                    svector Q(geometry.getQ());
                    Q /= Q.norm2();
                    if (Q == m_Q)
                      {
                        Quaternion q(geometry.getSampleQuaternion());
                        q *= m_geometry_E4C.getSampleQuaternion().conjugate();

                        svector axe(q.getAxe());
                        //if axe = (0,0,0), we get back to the initial position so return true.
                        if (axe == svector())
                            return true;
                        else
                            return axe.vectorialProduct(m_Q) == svector();

                      }
                    else
                        return false;
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

                    return value;
                  }

                void
                Psi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
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
                        geometry::eulerian4C::Vertical g1(omega, chi, phi, two_theta);

                        //2nd solution -pi<chi<0
                        omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
                        chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                        phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
                        geometry::eulerian4C::Vertical g2(omega, chi, phi, two_theta);

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
                    Vertical::toStream(flux);
                    m_Q.toStream(flux);

                    return flux;
                  }

                istream &
                Psi::fromStream (istream & flux)
                  {
                    Vertical::fromStream(flux);
                    m_Q.fromStream(flux);

                    return flux;
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
