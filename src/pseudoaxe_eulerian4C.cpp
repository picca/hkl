#include "pseudoaxe_eulerian4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian4C {
            namespace vertical {

                /*****************/
                /* PSI PSEUDOAXE */
                /*****************/
                Psi::Psi(void) :
                  PseudoAxe<geometry::eulerian4C::Vertical>()
                {
                  set_name ("psi");
                  set_description ("psi is the angle of rotation around the Q vector.");
                }

                Psi::Psi(Psi const & psi) :
                  PseudoAxe<geometry::eulerian4C::Vertical>(psi),
                  m_Q(psi.m_Q)
                {}

                Psi::~Psi(void)
                  {}

                void
                Psi::initialize(geometry::eulerian4C::Vertical const & geometry) throw (HKLException)
                  {
                    m_geometry = geometry;
                    m_Q = m_geometry.getQ();
                    double norm2 = m_Q.norm2();
                    if (norm2 > constant::math::epsilon_0)
                      {
                        m_Q /= norm2;
                        m_wasInitialized = true;
                      }
                    else
                        HKLEXCEPTION("the Q vector is null", "Check the wave length.");
                  }

                bool
                Psi::get_isValid(geometry::eulerian4C::Vertical const & geometry) const
                  {
                    svector Q(geometry.getQ());
                    double norm2 = Q.norm2();
                    if (norm2 > constant::math::epsilon_0)
                      {
                        Q /= norm2;
                        if (Q == m_Q)
                          {
                            Quaternion q(geometry.getSampleQuaternion());
                            q *= m_geometry.getSampleQuaternion().conjugate();

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
                    else
                        return false;
                  }

                double
                Psi::get_value(geometry::eulerian4C::Vertical const & geometry) const throw (HKLException)
                  {
                    if (Psi::get_isValid(geometry))
                      {
                        double value;
                        svector psi_axe(m_Q);

                        Quaternion qpsi = geometry.getSampleQuaternion();
                        Quaternion qpsi0 = m_geometry.getSampleQuaternion().conjugate();
                        qpsi *= qpsi0;

                        qpsi.getAngleAndAxe(value, psi_axe);

                        return value;
                      }
                    else
                        HKLEXCEPTION("the current geometry is not compatible with the \"psi\" initialization",
                                     "please initilize the pseudoAxe \"psi\"");
                  }

                void
                Psi::set_value(geometry::eulerian4C::Vertical & g,
                               double const & value) const throw (HKLException)
                  {
                    Quaternion qm0 = m_geometry.getSampleQuaternion();
                    Quaternion q(value, m_Q);
                    q *= qm0;
                    smatrix M = q.asMatrix();

                    double omega;
                    double chi;
                    double phi;
                    double tth;
                    if (fabs (M.get(0, 1)) < constant::math::epsilon_0
                        && fabs (M.get(1, 0)) < constant::math::epsilon_0
                        && fabs (M.get(2, 1)) < constant::math::epsilon_0
                        && fabs (M.get(1, 2)) < constant::math::epsilon_0)
                      {
                        omega = g.m_omega.get_value();
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
                        g.m_chi.set_value(chi);
                        g.m_phi.set_value(phi);
                      }
                    else
                      {
                        //1st solution 0<chi<pi
                        omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
                        chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                        phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
                        tth = m_geometry.m_tth.get_value();
                        geometry::eulerian4C::Vertical g1(omega, chi, phi, tth);

                        //2nd solution -pi<chi<0
                        omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
                        chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                        phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
                        geometry::eulerian4C::Vertical g2(omega, chi, phi, tth);

                        double d1 = g.getDistance(g1);
                        double d2 = g.getDistance(g2);
                        if (d1 < d2)
                          {
                            g.m_omega.set_value(g1.m_omega.get_value());
                            g.m_chi.set_value(g1.m_chi.get_value());
                            g.m_phi.set_value(g1.m_phi.get_value());
                          }
                        else
                          {
                            g.m_omega.set_value(g2.m_omega.get_value());
                            g.m_chi.set_value(g2.m_chi.get_value());
                            g.m_phi.set_value(g2.m_phi.get_value());
                          }
                        g.m_tth.set_value(tth);
                      }
                  }

                ostream &
                Psi::toStream(ostream & flux) const
                  {
                    PseudoAxe<geometry::eulerian4C::Vertical>::toStream(flux);
                    m_Q.toStream(flux);

                    return flux;
                  }

                istream &
                Psi::fromStream (istream & flux)
                  {
                    PseudoAxe<geometry::eulerian4C::Vertical>::fromStream(flux);
                    m_Q.fromStream(flux);

                    return flux;
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
