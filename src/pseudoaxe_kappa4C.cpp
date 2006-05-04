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
                Omega::initialize(Geometry const & geometry)
                  {
                    set_wasInitialized(true);
                  }

                bool
                Omega::get_isValid(Geometry const & geometry) const
                  {
                    return true;
                  }

                double const 
                Omega::get_value(Geometry const & geometry)
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    double const & komega = geometry.get_axe("komega").get_value();
                    double const & kappa = geometry.get_axe("kappa").get_value();

                    return komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                  }

                void
                Omega::set_value(Geometry & geometry,
                                 double const & value) throw (HKLException)
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
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
                Chi::initialize(Geometry const & geometry)
                  {
                    set_wasInitialized(true);
                  }

                bool
                Chi::get_isValid(Geometry const & geometry) const
                  {
                    return true;
                  }

                double const 
                Chi::get_value(Geometry const & geometry)
                  {
                    double const & kappa = geometry.get_axe("kappa").get_value();
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();

                    return -2 * asin(sin(kappa/2.) * sin(alpha));
                  }

                void
                Chi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
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
                        throw HKLException("chi is to big",
                                           "|chi| <= 2 * alpha",
                                           "Chi::set_value");
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
                Phi::initialize(Geometry const & geometry)
                  {
                    set_wasInitialized(true);
                  }

                bool
                Phi::get_isValid(Geometry const & geometry) const
                  {
                    return true;
                  }

                double const 
                Phi::get_value(Geometry const & geometry)
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    double const & kappa = geometry.get_axe("kappa").get_value();
                    double const & kphi = geometry.get_axe("kphi").get_value();

                    return kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                  }

                void
                Phi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
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
                Psi::initialize(Geometry const & geometry)
                  {
                    m_E4C.setFromGeometry(static_cast<geometry::kappa4C::Vertical const &>(geometry));
#ifdef MSVC6
                    m_psi.set_valueList(m_psi.get_valueList());
                    m_psi.initialize(m_E4C);
#else
                    pseudoAxe::eulerian4C::vertical::Psi::initialize(m_E4C);
#endif
                  }

                bool
                Psi::get_isValid(Geometry const & geometry) const
                  {
                    m_E4C.setFromGeometry(static_cast<geometry::kappa4C::Vertical const &>(geometry));
#ifdef MSVC6
                    m_psi.set_valueList(m_psi.get_valueList());
                    return m_psi.get_isValid(m_E4C);
#else
                    return pseudoAxe::eulerian4C::vertical::Psi::get_isValid(m_E4C);
#endif
                  }

                double const 
                Psi::get_value(Geometry const & geometry)
                  {
                    m_E4C.setFromGeometry(static_cast<geometry::kappa4C::Vertical const &>(geometry));
#ifdef MSVC6
                    m_psi.set_valueList(m_psi.get_valueList());
                    return m_psi.get_value(m_E4C);
#else
                    return pseudoAxe::eulerian4C::vertical::Psi::get_value(m_E4C);
#endif
                  }

                void
                Psi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
                    m_E4C.setFromGeometry(static_cast<geometry::kappa4C::Vertical &>(geometry));
#ifdef MSVC6
                    m_psi.set_valueList(m_psi.get_valueList());
                    m_psi.set_value(m_E4C, value);
#else
                    pseudoAxe::eulerian4C::vertical::Psi::set_value(m_E4C, value);
#endif
                    static_cast<geometry::kappa4C::Vertical &>(geometry).setFromGeometry(m_E4C);
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
