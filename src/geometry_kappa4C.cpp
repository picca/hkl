#include "geometry_kappa4C.h"

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

    } // namespace geometry
} // namespace hkl
