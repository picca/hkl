#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa4C {

            Vertical::Vertical(double alpha) :
              PseudoAxe()
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
                    double const & alpha = m_geometry_K4C->get_alpha();
                    double const & komega = geometry.get_axe("komega").get_value();
                    double const & kappa = geometry.get_axe("kappa").get_value();

                    return komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                  }

                void
                Omega::set_value(Geometry & geometry,
                                 double const & value) throw (HKLException)
                  {
                    double const & alpha = m_geometry_K4C->get_alpha();
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
                    double const & alpha = m_geometry_K4C->get_alpha();

                    return -2 * asin(sin(kappa/2.) * sin(alpha));
                  }

                void
                Chi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
                    double const & alpha = m_geometry_K4C->get_alpha();
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
                    double const & alpha = m_geometry_K4C->get_alpha();
                    double const & kappa = geometry.get_axe("kappa").get_value();
                    double const & kphi = geometry.get_axe("kphi").get_value();

                    return kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                  }

                void
                Phi::set_value(Geometry & geometry,
                               double const & value) throw (HKLException)
                  {
                    double const & alpha = m_geometry_K4C->get_alpha();
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

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
