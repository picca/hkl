#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {
        namespace twoC {

            Vertical::Vertical(void)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
              }

            Vertical::Vertical(Vertical const & geometry)
            : Geometry(geometry)
              {}

            Vertical::Vertical(double const & omega, double const & two_theta)
            : Geometry()
              {
                addSampleAxe(Axe("omega", svector(0., 1., 0.), -1, omega));
                addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1, two_theta));
              }

            Vertical::~Vertical(void)
              {}

            void
            Vertical::setAngles(double const & omega, double const & two_theta)
              {
                get_axe("omega").set_value(omega);
                get_axe("2theta").set_value(two_theta);
              }

            void
            Vertical::setFromGeometry(eulerian4C::Vertical const & E4C) throw (HKLException)
              {
                if (fabs(E4C.get_axe("chi").get_value()) < constant::math::epsilon_1
                    && fabs(E4C.get_axe("phi").get_value()) < constant::math::epsilon_1)
                  {
                    m_source = E4C.get_source();
                    get_axe("omega").set_value(E4C.get_axe("omega").get_value());
                    get_axe("2theta").set_value(E4C.get_axe("2theta").get_value());
                  }
                else
                    throw HKLException("\"chi\" and/or \"phi\" axe(s) are wrong",
                                       "\"chi\" = \"phi\" must be set to zero",
                                       "geometry::twoC::Vertical::setFromGeometry(eulerian4C::Vertical const &)");
              }

            void
            Vertical::setFromGeometry(kappa4C::Vertical const & K4C) throw (HKLException)
              {
                if (fabs(K4C.get_axe("kappa").get_value()) < constant::math::epsilon_1
                    && fabs(K4C.get_axe("kphi").get_value()) < constant::math::epsilon_1)
                  {
                    m_source = K4C.get_source();
                    get_axe("omega").set_value(K4C.get_axe("komega").get_value());
                    get_axe("2theta").set_value(K4C.get_axe("2theta").get_value());
                  }
                else
                    throw HKLException("\"kappa\" and/or \"kphi\" axe(s) are wrong",
                                       "\"kappa\" = \"kphi\" must be set to zero",
                                       "geometry::twoC::Vertical::setFromGeometry(kappa4C::Vertical const &)");
              }

            void
            Vertical::setFromGeometry(Kappa6C const & K6C) throw (HKLException)
              {
                if (fabs(K6C.get_axe("gamma").get_value()) < constant::math::epsilon_1
                    && fabs(K6C.get_axe("mu").get_value()) < constant::math::epsilon_1
                    && fabs(K6C.get_axe("kappa").get_value()) < constant::math::epsilon_1
                    && fabs(K6C.get_axe("kphi").get_value()) < constant::math::epsilon_1)
                  {
                    m_source = K6C.get_source();
                    get_axe("omega").set_value(K6C.get_axe("komega").get_value());
                    get_axe("2theta").set_value(K6C.get_axe("delta").get_value());
                  }
                else
                    throw HKLException("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" axe(s) are wrong",
                                       "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" must be set to zero",
                                       "geometry::kappa6C::Vertical::setFromGeometry(Kappa6C const &)");
              }

        } // namespace twoC
    } // namespace geometry
} // namespace hkl
