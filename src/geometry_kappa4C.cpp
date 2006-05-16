#include <sstream>

#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "constants.h"

namespace hkl {
    namespace geometry {
        namespace kappa4C {

            /*********************/
            /* Vertical Geometry */
            /*********************/
            Vertical::Vertical(double alpha) :
              Kappa(alpha)
            {
              addSampleAxe(Axe("komega", svector(0., 1., 0.), -1));
              addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1));
              addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1));
              addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
            }

            Vertical::Vertical(double alpha, double komega, double kappa, double kphi, double two_theta) :
              Kappa(alpha)
            {
              addSampleAxe(Axe("komega", svector(0., 1., 0.), -1, komega));
              addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa));
              addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1, kphi));
              addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
            }

            Vertical::~Vertical(void)
              {}

            void
            Vertical::setFromGeometry(Geometry const & geometry) throw (HKLException)
              {
                const type_info & type = typeid(geometry);
                // Eulerian4C::Vertical
                if (type == typeid(geometry::eulerian4C::Vertical))
                  {
                    double const & chi = geometry.get_axe("chi").get_value();
                    if (fabs(chi) <= 2 * m_alpha)
                      {
                        double const & omega = geometry.get_axe("omega").get_value();
                        double const & phi = geometry.get_axe("phi").get_value();
                        double const & two_theta = geometry.get_axe("2theta").get_value();

                        double p = asin(tan(chi/2.)/tan(m_alpha));
                        double komega = omega + p - constant::math::pi/2.;
                        double kappa = -2 * asin(sin(chi/2.)/sin(m_alpha));
                        double kphi = phi + p + constant::math::pi/2.;

                        m_source = geometry.get_source();
                        get_axe("komega").set_value(komega);
                        get_axe("kappa").set_value(kappa);
                        get_axe("kphi").set_value(kphi);
                        get_axe("2theta").set_value(two_theta);
                      }
                    else
                      {
                        ostringstream description;
                        description << "\"chi\"(" << chi * constant::math::radToDeg << ") must be lower than " << 2*m_alpha*constant::math::radToDeg;
                        throw HKLException("\"chi\" is unreachable",
                                           description.str(),
                                           "geometry::kappa4C::Vertical::setFromGeometry(geometry::eulerian4C::Vertical const &)");
                      }
                  }
                // Kappa6C
                else if (type == typeid(geometry::Kappa6C))
                  {
                    double const & mu = geometry.get_axe("mu").get_value();
                    double const & gamma = geometry.get_axe("gamma").get_value();
                    if (!mu && !gamma)
                      {
                        get_axe("komega").set_value(geometry.get_axe("komega").get_value());
                        get_axe("kappa").set_value(geometry.get_axe("kappa").get_value());
                        get_axe("kphi").set_value(geometry.get_axe("kphi").get_value());
                        get_axe("2theta").set_value(geometry.get_axe("delta").get_value());
                      }
                    else
                      {
                        ostringstream description;
                        if (mu && gamma)
                          {
                            description << "mu and gamma must be zero";
                          }
                        else if (mu)
                          {
                            description << "mu must be zero";
                          }
                        else if (gamma)
                          {
                            description << "gamma must be zero";
                          }
                        throw HKLException("kappa6C geometry is not compatible with a kappa4C::Vertical geometry.",
                                           description.str(),
                                           "geometry::kappa4C::Vertical::setFromGeometry(geometry::Kappa6C const &)");
                      }
                  }

              }
            
            /***********************/
            /* Horizontal Geometry */
            /***********************/
            Horizontal::Horizontal(double alpha) :
              Kappa(alpha)
            {
              addSampleAxe(Axe("komega", svector(0., 0., 1.), 1));
              addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1));
              addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1));
              addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
            }

            Horizontal::Horizontal(double alpha, double komega, double kappa, double kphi, double two_theta) :
              Kappa(alpha)
            {
              addSampleAxe(Axe("komega", svector(0., 0., 1.), 1, komega));
              addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa));
              addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1, kphi));
              addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
            }

            Horizontal::~Horizontal(void)
              {}

        } // namespace kappa4C
    } // namespace geometry
} // namespace hkl
