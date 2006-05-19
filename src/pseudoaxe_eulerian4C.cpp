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
                Psi::initialize(Geometry const & geometry) throw (HKLException)
                  {
                    m_geometry_E4C = dynamic_cast<geometry::eulerian4C::Vertical const &>(geometry);
                    m_Q = geometry.getQ();
                    double norm2 = m_Q.norm2();
                    if (norm2 > constant::math::epsilon_0)
                      {
                        m_Q /= norm2;
                        set_wasInitialized(true);
                      }
                    else
                        throw HKLException("the Q vector is null",
                                           "Check the wave length.",
                                           "");
                  }

                bool
                Psi::get_isValid(Geometry const & geometry) const
                  {
                    svector Q(geometry.getQ());
                    double norm2 = Q.norm2();
                    if (norm2 > constant::math::epsilon_0)
                      {
                        Q /= norm2;
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
                    else
                        return false;
                  }

                double
                Psi::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    if (get_isValid(geometry))
                      {
                        double value;
                        svector psi_axe(m_Q);

                        Quaternion qpsi = geometry.getSampleQuaternion();
                        Quaternion qpsi0 = m_geometry_E4C.getSampleQuaternion().conjugate();
                        qpsi *= qpsi0;

                        qpsi.getAngleAndAxe(value, psi_axe);

                        return value;
                      }
                    else
                        throw HKLException("the current geometry is not compatible with the \"psi\" initialization",
                                           "please initilize the pseudoAxe \"psi\"",
                                           "hkl::pseudoAxe::eulerian4C::vertical::Psi::get_value(Geometry const &)");
                  }

                void
                Psi::set_value(Geometry & geometry,
                               double const & value) const throw (HKLException)
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

                namespace twoC {

                    /*********/
                    /* Th2th */
                    /*********/
                    Th2th::Th2th(void) :
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::twoC::vertical::Th2th()
#endif
                    {
                      set_name("th2th");
#ifdef MSVC6
                      set_description(m_th2th.get_description());
                      set_valueList(m_th2th.get_valueList());
#endif
                    }

                    Th2th::~Th2th(void)
                      {}

                    void
                    Th2th::initialize(Geometry const & geometry) throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_th2th.set_valueList(get_valueList());
                        m_th2th.initialize(m_twoC);
#else
                        pseudoAxe::twoC::vertical::Th2th::initialize(m_twoC);
#endif
                      }

                    bool
                    Th2th::get_isValid(Geometry const & geometry) const
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_th2th.set_valueList(get_valueList());
                        return m_th2th.get_isValid(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Th2th::get_isValid(m_twoC);
#endif
                      }

                    double
                    Th2th::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_th2th.set_valueList(get_valueList());
                        return m_th2th.get_value(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Th2th::get_value(m_twoC);
#endif
                      }

                    void
                    Th2th::set_value(Geometry & geometry,
                                     double const & value) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_th2th.set_valueList(get_valueList());
                        m_th2th.set_value(m_twoC, value);
#else
                        pseudoAxe::twoC::vertical::Th2th::set_value(m_twoC, value);
#endif
                        dynamic_cast<geometry::eulerian4C::Vertical &>(geometry).setFromGeometry(m_twoC, false);
                      }

                    /*********/
                    /* Q2th */
                    /*********/
                    Q2th::Q2th(void) :
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::twoC::vertical::Q2th()
#endif
                    {
                      set_name("q2th");
#ifdef MSVC6
                      set_description(m_q2th.get_description());
                      set_valueList(m_q2th.get_valueList());
#endif
                    }

                    Q2th::~Q2th(void)
                      {}

                    void
                    Q2th::initialize(Geometry const & geometry) throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q2th.set_valueList(get_valueList());
                        m_q2th.initialize(m_twoC);
#else
                        pseudoAxe::twoC::vertical::Q2th::initialize(m_twoC);
#endif
                      }

                    bool
                    Q2th::get_isValid(Geometry const & geometry) const
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q2th.set_valueList(get_valueList());
                        return m_q2th.get_isValid(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Q2th::get_isValid(m_twoC);
#endif
                      }

                    double
                    Q2th::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q2th.set_valueList(get_valueList());
                        return m_q2th.get_value(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Q2th::get_value(m_twoC);
#endif
                      }

                    void
                    Q2th::set_value(Geometry & geometry,
                                    double const & value) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q2th.set_valueList(get_valueList());
                        m_q2th.set_value(m_twoC, value);
#else
                        pseudoAxe::twoC::vertical::Q2th::set_value(m_twoC, value);
#endif
                        dynamic_cast<geometry::eulerian4C::Vertical &>(geometry).setFromGeometry(m_twoC, false);
                      }

                    /*****/
                    /* Q */
                    /****/
                    Q::Q(void) :
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::twoC::vertical::Q()
#endif
                    {
                      set_name("q");
#ifdef MSVC6
                      set_description(m_q.get_description());
                      set_valueList(m_q.get_valueList());
#endif
                    }

                    Q::~Q(void)
                      {}

                    void
                    Q::initialize(Geometry const & geometry) throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q.set_valueList(get_valueList());
                        m_q.initialize(m_twoC);
#else
                        pseudoAxe::twoC::vertical::Q::initialize(m_twoC);
#endif
                      }

                    bool
                    Q::get_isValid(Geometry const & geometry) const
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q.set_valueList(get_valueList());
                        return m_q.get_isValid(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Q::get_isValid(m_twoC);
#endif
                      }

                    double
                    Q::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q.set_valueList(get_valueList());
                        return m_q.get_value(m_twoC);
#else
                        return pseudoAxe::twoC::vertical::Q::get_value(m_twoC);
#endif
                      }

                    void
                    Q::set_value(Geometry & geometry,
                                 double const & value) const throw (HKLException)
                      {
                        m_twoC.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_q.set_valueList(get_valueList());
                        m_q.set_value(m_twoC, value);
#else
                        pseudoAxe::twoC::vertical::Q::set_value(m_twoC, value);
#endif
                        dynamic_cast<geometry::eulerian4C::Vertical &>(geometry).setFromGeometry(m_twoC, false);
                      }

                } // namespace twoC
            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
