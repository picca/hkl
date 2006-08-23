#include "pseudoaxe_eulerian4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian4C {
            namespace vertical {

                /*****************/
                /* PSI PSEUDOAXE */
                /*****************/
                Psi::Psi(geometry::eulerian4C::Vertical & geometry) :
                  PseudoAxe<geometry::eulerian4C::Vertical>(geometry)
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
                Psi::initialize(void) throw (HKLException)
                  {
                    // check the geometry validity.
                    if (m_geometry.isValid())
                      {
                        double norm2 = m_geometry.getQ().norm2();
                        if (norm2 > constant::math::epsilon_0)
                          {
                            PseudoAxe<geometry::eulerian4C::Vertical>::initialize();
                            m_Q = m_geometry0.getQ();
                            m_Q /= norm2;
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the Q vector is null.";
                            HKLEXCEPTION(reason.str(), "Check the wave length.");
                          }
                      }
                  }

                double
                Psi::get_min(void) const
                  {
                    if (m_initialized)
                        return -constant::math::pi;
                    else
                        return 0;
                  }

                double
                Psi::get_max(void) const
                  {
                    if (m_initialized)
                        return constant::math::pi;
                    else
                        return 0;
                  }

                bool
                Psi::isValid(void) throw (HKLException)
                  {
                    bool valid = false;
                    if (PseudoAxe<geometry::eulerian4C::Vertical>::isValid())
                      {
                        svector Q(m_geometry.getQ());
                        double norm2 = Q.norm2();
                        // check that |Q| is non-null
                        if (norm2 > constant::math::epsilon_0)
                          {
                            Q /= norm2;
                            if (Q == m_Q)
                              {
                                Quaternion q(m_geometry.getSampleQuaternion());
                                q *= m_geometry0.getSampleQuaternion().conjugate();

                                svector axe(q.getAxe());
                                //if axe = (0,0,0), we get back to the initial position so return true.
                                if (axe == svector())
                                    valid = true;
                                else
                                    valid = axe.vectorialProduct(m_Q) == svector();

                              }
                          }
                      }
                    if (valid)
                        m_writable = true;
                    else
                      {
                        m_writable = false;
                        HKLEXCEPTION("The current geometry is not compatible with the pseudoAxe initialisation.","Please re-initialize it.");
                      }
                    return valid;
                  }

                double
                Psi::get_value(void) throw (HKLException)
                  {
                    if (Psi::isValid())
                      {
                        double value;
                        svector psi_axe(m_Q);

                        Quaternion qpsi = m_geometry.getSampleQuaternion();
                        Quaternion qpsi0 = m_geometry0.getSampleQuaternion();
                        qpsi *= qpsi0.conjugate();

                        qpsi.getAngleAndAxe(value, psi_axe);

                        return value;
                      }
                  }

                void
                Psi::set_value(double const & value) throw (HKLException)
                  {
                    if (Psi::isValid())
                      {
                        Quaternion qm0 = m_geometry0.getSampleQuaternion();
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
                            omega = m_geometry.m_omega.get_value();
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
                            m_geometry.m_chi.set_value(chi);
                            m_geometry.m_phi.set_value(phi);
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

                            double d1 = m_geometry.getDistance(g1);
                            double d2 = m_geometry.getDistance(g2);
                            if (d1 < d2)
                              {
                                m_geometry.m_omega.set_value(g1.m_omega.get_value());
                                m_geometry.m_chi.set_value(g1.m_chi.get_value());
                                m_geometry.m_phi.set_value(g1.m_phi.get_value());
                              }
                            else
                              {
                                m_geometry.m_omega.set_value(g2.m_omega.get_value());
                                m_geometry.m_chi.set_value(g2.m_chi.get_value());
                                m_geometry.m_phi.set_value(g2.m_phi.get_value());
                              }
                            m_geometry.m_tth.set_value(tth);
                          }
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
