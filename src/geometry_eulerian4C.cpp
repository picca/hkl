#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "pseudoaxe_eulerian4C.h"

namespace hkl {
    namespace geometry {
        namespace eulerian4C {

            Vertical::Vertical(void)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
                addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
                addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
              }

            Vertical::Vertical(Vertical const & geometry)
            : Geometry(geometry)
              {}

            Vertical::Vertical(double const & omega, double const & chi, double const & phi, double const & two_theta)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 1., 0.), -1, omega));
                addSampleAxe(Axe("chi", svector(1., 0., 0.), 1, chi));
                addSampleAxe(Axe("phi", svector(0., 1., 0.), -1, phi));

                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
              }

            Vertical::~Vertical(void)
              {}

            void
            Vertical::setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta)
              {
                get_axe("omega").set_value(omega);
                get_axe("chi").set_value(chi);
                get_axe("phi").set_value(phi);
                get_axe("2theta").set_value(two_theta);
              }

            void
            Vertical::setFromGeometry(Geometry const & geometry) throw (HKLException)
              {
                // kappa4C::Vertical
                const type_info & type = typeid(geometry);
                if (type == typeid(geometry::kappa4C::Vertical))
                  {
                    double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                    double const & komega = geometry.get_axe("komega").get_value();
                    double const & kappa = geometry.get_axe("kappa").get_value();
                    double const & kphi = geometry.get_axe("kphi").get_value();
                    double const & two_theta = geometry.get_axe("2theta").get_value();

                    double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                    double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                    double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                    m_source = geometry.get_source();
                    get_axe("omega").set_value(omega);
                    get_axe("chi").set_value(chi);
                    get_axe("phi").set_value(phi);
                    get_axe("2theta").set_value(two_theta);
                  }
                // kappa6C
                else if (type == typeid(geometry::Kappa6C))
                  {
                    if (fabs(geometry.get_axe("gamma").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("mu").get_value()) < constant::math::epsilon_1)
                      {
                        double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                        double const & komega = geometry.get_axe("komega").get_value();
                        double const & kappa = geometry.get_axe("kappa").get_value();
                        double const & kphi = geometry.get_axe("kphi").get_value();
                        double const & two_theta = geometry.get_axe("delta").get_value();

                        double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                        m_source = geometry.get_source();
                        get_axe("omega").set_value(omega);
                        get_axe("chi").set_value(chi);
                        get_axe("phi").set_value(phi);
                        get_axe("2theta").set_value(two_theta);
                      }
                    else
                        throw HKLException("\"gamma\" and/or \"mu\" axe(s) are wrong",
                                           "\"gamma\" = \"mu\" must be set to zero",
                                           "geometry::eulerian4C::Vertical::setFromGeometry");
                  }
              }

            Horizontal::Horizontal(void)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 0., 1.), 1));
                addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
                addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
              }

            Horizontal::Horizontal(Geometry const & geometry)
            : Geometry(geometry)
              {}

            Horizontal::Horizontal(double omega, double chi, double phi, double two_theta)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 0., 1.), 1, omega));
                addSampleAxe(Axe("chi", svector(1., 0., 0.), 1, chi));
                addSampleAxe(Axe("phi", svector(0., 1., 0.), -1, phi));

                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
              }

            Horizontal::~Horizontal(void)
              {}

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl
