#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa4C {

            Vertical::Vertical(double alpha)
              {
                m_geometry_K4C = new geometry::kappa4C::Vertical(alpha);
              }

            Vertical::~Vertical(void)
              {
                delete m_geometry_K4C;
              }

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                PseudoAxe::toStream (flux);
                m_geometry_K4C->toStream (flux);

                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                PseudoAxe::fromStream (flux);
                m_geometry_K4C->fromStream (flux);

                return flux;
              }

            namespace vertical {
                /*******************/
                /* OMEGA PSEUDOAXE */
                /*******************/
                Omega::Omega(double alpha) :
                  Vertical(alpha)
                {
                  set_name ("omega");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Omega::~Omega(void)
                  {}

                void
                Omega::initialize(Geometry const & geometry) throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        Omega::set_wasInitialized(true);
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Omega::get_isValid(Geometry const & geometry) const
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Omega::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & komega = geometry.get_axe("komega").get_value();
                        double const & kappa = geometry.get_axe("kappa").get_value();

                        return komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Omega::set_value(Geometry & geometry,
                                 double const & value) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double komega;
                        double kappa = geometry.get_axe("kappa").get_value();
                        double kphi = geometry.get_axe("kphi").get_value();

                        double const & omega = value;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        geometry.get_axe("komega").set_value(komega);
                        geometry.get_axe("kappa").set_value(kappa);
                        geometry.get_axe("kphi").set_value(kphi);
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                /*****************/
                /* CHI PSEUDOAXE */
                /*****************/
                Chi::Chi(double alpha) :
                  Vertical(alpha)
                {
                  set_name ("chi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Chi::~Chi(void)
                  {}

                void
                Chi::initialize(Geometry const & geometry) throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        Chi::set_wasInitialized(true);
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Chi::get_isValid(Geometry const & geometry) const
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Chi::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & kappa = geometry.get_axe("kappa").get_value();
                        return -2 * asin(sin(kappa/2.) * sin(alpha));
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Chi::set_value(Geometry & geometry,
                               double const & value) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        if (fabs(value) <= 2 * alpha)
                          {

                            double komega = geometry.get_axe("komega").get_value();
                            double kappa = geometry.get_axe("kappa").get_value();
                            double kphi = geometry.get_axe("kphi").get_value();

                            double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                            double const & chi = value;
                            double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                            double p = asin(tan(chi/2.)/tan(alpha));
                            komega = omega + p - constant::math::pi/2.;
                            kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                            kphi = phi + p + constant::math::pi/2.;

                            geometry.get_axe("komega").set_value(komega);
                            geometry.get_axe("kappa").set_value(kappa);
                            geometry.get_axe("kphi").set_value(kphi);
                          }
                        else
                            HKLEXCEPTION("chi is to big",
                                         "|chi| <= 2 * alpha");
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                /*****************/
                /* PHI PSEUDOAXE */
                /*****************/
                Phi::Phi(double alpha) :
                  Vertical(alpha)
                {
                  set_name ("phi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Phi::~Phi(void)
                  {}

                void
                Phi::initialize(Geometry const & geometry) throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        Phi::set_wasInitialized(true);
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Phi::get_isValid(Geometry const & geometry) const
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Phi::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & kappa = geometry.get_axe("kappa").get_value();
                        double const & kphi = geometry.get_axe("kphi").get_value();

                        return kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Phi::set_value(Geometry & geometry,
                               double const & value) const throw (HKLException)
                  {
                    double const & alpha = dynamic_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double komega = geometry.get_axe("komega").get_value();
                        double kappa = geometry.get_axe("kappa").get_value();
                        double kphi;

                        double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double const & phi = value;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        geometry.get_axe("komega").set_value(komega);
                        geometry.get_axe("kappa").set_value(kappa);
                        geometry.get_axe("kphi").set_value(kphi);
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                /*******/
                /* PSI */
                /*******/
                Psi::Psi(double alpha) :
#ifdef MSVC6
                  PseudoAxe()
#else
                  pseudoAxe::eulerian4C::vertical::Psi()
#endif
                {
                  set_name("psi");
#ifdef MSVC6
                  set_description(m_psi.get_description());
                  set_valueList(m_psi.get_valueList());
#endif
                }

                Psi::~Psi(void)
                  {}

                void
                Psi::initialize(Geometry const & geometry) throw (HKLException)
                  {
                    m_E4C.setFromGeometry(geometry, false);
#ifdef MSVC6
                    m_psi.set_valueList(get_valueList());
                    m_psi.initialize(m_E4C);
#else
                    pseudoAxe::eulerian4C::vertical::Psi::initialize(m_E4C);
#endif
                  }

                bool
                Psi::get_isValid(Geometry const & geometry) const
                  {
                    m_E4C.setFromGeometry(geometry, false);
#ifdef MSVC6
                    m_psi.set_valueList(get_valueList());
                    return m_psi.get_isValid(m_E4C);
#else
                    return pseudoAxe::eulerian4C::vertical::Psi::get_isValid(m_E4C);
#endif
                  }

                double
                Psi::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    m_E4C.setFromGeometry(geometry, false);
#ifdef MSVC6
                    m_psi.set_valueList(get_valueList());
                    return m_psi.get_value(m_E4C);
#else
                    return pseudoAxe::eulerian4C::vertical::Psi::get_value(m_E4C);
#endif
                  }

                void
                Psi::set_value(Geometry & geometry,
                               double const & value) const throw (HKLException)
                  {
                    m_E4C.setFromGeometry(geometry, false);
#ifdef MSVC6
                    m_psi.set_valueList(get_valueList());
                    m_psi.set_value(m_E4C, value);
#else
                    pseudoAxe::eulerian4C::vertical::Psi::set_value(m_E4C, value);
#endif
                    dynamic_cast<geometry::kappa4C::Vertical &>(geometry).setFromGeometry(m_E4C, false);
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
                        geometry.setFromGeometry(m_twoC, false);
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
                        geometry.setFromGeometry(m_twoC, false);
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
                        geometry.setFromGeometry(m_twoC, false);
                      }

                } // namespace twoC
            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
