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

    } // namespace geometry
} // namespace hkl
