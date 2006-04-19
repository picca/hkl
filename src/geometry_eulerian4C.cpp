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

        Vertical::Vertical(Geometry const & geometry)
        : Geometry(geometry)
          {}

        Vertical::Vertical(double omega, double chi, double phi, double two_theta)
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
        Vertical::setFromGeometry(kappa4C::Vertical const & K4C)
          {
            double const & alpha = K4C.get_alpha();
            double const & komega = K4C.get_axe("komega").get_value();
            double const & kappa = K4C.get_axe("kappa").get_value();
            double const & kphi = K4C.get_axe("kphi").get_value();
            double const & two_theta = K4C.get_axe("2theta").get_value();

            double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
            double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
            double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

            m_source = K4C.get_source();
            get_axe("omega").set_value(omega);
            get_axe("chi").set_value(chi);
            get_axe("phi").set_value(phi);
            get_axe("2theta").set_value(two_theta);
          }

        void
        Vertical::setFromGeometry(Kappa6C const & K6C)
          {
            double const & alpha = K6C.get_alpha();
            double const & komega = K6C.get_axe("komega").get_value();
            double const & kappa = K6C.get_axe("kappa").get_value();
            double const & kphi = K6C.get_axe("kphi").get_value();
            double const & two_theta = K6C.get_axe("2theta").get_value();

            double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
            double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
            double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

            m_source = K6C.get_source();
            get_axe("omega").set_value(omega);
            get_axe("chi").set_value(chi);
            get_axe("phi").set_value(phi);
            get_axe("2theta").set_value(two_theta);
          }

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl
