#include <sstream>

#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {

        Kappa6C::Kappa6C(double alpha) :
          Kappa(alpha)
        {
          // Sample
          addSampleAxe(Axe("mu", svector(0., 0., 1.), 1));
          addSampleAxe(Axe("komega", svector(0., 1., 0.), -1));
          addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1));
          addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1));

          // Detector
          addDetectorAxe(Axe("gamma", svector(0., 0., 1.), 1));
          addDetectorAxe(Axe("delta", svector(0., 1., 0.), -1));
        }

        Kappa6C::Kappa6C(double alpha, double mu, double komega, double kappa, double kphi, double gamma, double delta) :
          Kappa(alpha)
        {
          // Sample
          addSampleAxe(Axe("mu", svector(0., 0., 1.), 1, mu));
          addSampleAxe(Axe("komega", svector(0., 1., 0.), -1, komega));
          addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa));
          addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1, kphi));

          // Detector
          addDetectorAxe(Axe("gamma", svector(0., 0., 1.), 1, gamma));
          addDetectorAxe(Axe("delta", svector(0., 1., 0.), -1, delta));
        }

        Kappa6C::~Kappa6C(void)
          {}

        void
        Kappa6C::setFromGeometry(eulerian4C::Vertical const & E4C) throw (HKLException)
          {
            double const & chi = E4C.get_axe("chi").get_value();
            if (chi <= 2 * m_alpha)
              {
                double const & omega = E4C.get_axe("omega").get_value();
                double const & phi = E4C.get_axe("phi").get_value();
                double const & two_theta = E4C.get_axe("2theta").get_value();

                double p = asin(tan(chi/2.)/tan(m_alpha));
                double komega = omega + p - constant::math::pi/2.;
                double kappa = -2 * asin(sin(chi/2.)/sin(m_alpha));
                double kphi = phi + p + constant::math::pi/2.;

                m_source = E4C.get_source();

                get_axe("mu").set_value(0.);
                get_axe("komega").set_value(komega);
                get_axe("kappa").set_value(kappa);
                get_axe("kphi").set_value(kphi);
                get_axe("gamma").set_value(0.);
                get_axe("delta").set_value(two_theta);
              }
            else
              {
                ostringstream description;
                description << "\"chi\" must be lower than " << 2*m_alpha*constant::math::radToDeg;
                throw HKLException("\"chi\" is unreachable",
                                   description.str(),
                                   "geometry::Kappa6C::setFromGeometry");
              }
          }

    } // namespace geometry
} // namespace hkl
