#include "geometry_kappa4C.h"
#include "constants.h"

namespace hkl {
    namespace geometry {

        Kappa4C::Kappa4C(double alpha) :
          Kappa(alpha)
        {
          addSampleAxe(Axe("komega", svector(0., 1., 0.), -1));
          addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1));
          addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1));
          addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
        }

        Kappa4C::Kappa4C(double alpha, double komega, double kappa, double kphi, double two_theta) :
          Kappa(alpha)
        {
          addSampleAxe(Axe("komega", svector(0., 1., 0.), -1, komega));
          addSampleAxe(Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa));
          addSampleAxe(Axe("kphi", svector(0., 1., 0.), -1, kphi));
          addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
        }

        Kappa4C::~Kappa4C(void)
          {}

        void
        Kappa4C::setFromE4C(Eulerian4C const & E4C)
          {
            double const & omega = E4C.get_axe("omega").get_value();
            double const & chi = E4C.get_axe("chi").get_value();
            double const & phi = E4C.get_axe("phi").get_value();
            double const & two_theta = E4C.get_axe("2theta").get_value();
            
            double p = asin(tan(chi/2.)/tan(m_alpha));
            double komega = omega + p - constant::math::pi/2.;
            double kappa = -2 * asin(sin(chi/2.)/sin(m_alpha));
            double kphi = phi + p + constant::math::pi/2.;

            m_source = E4C.get_source();
            get_axe("komega").set_value(komega);
            get_axe("kappa").set_value(kappa);
            get_axe("kphi").set_value(kphi);
            get_axe("2theta").set_value(two_theta);
          }

    } // namespace geometry
} // namespace hkl
