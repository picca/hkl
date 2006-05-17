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
            Vertical::setFromGeometry(Geometry const & geometry) throw (HKLException)
              {
                double omega = 0.;
                double two_theta = 0.;

                // update the source
                m_source = geometry.get_source();

                const type_info & type = typeid(geometry);
                // twoC::Vertical
                if (type == typeid(geometry::twoC::Vertical))
                  {
                    omega = geometry.get_axe("omega").get_value();
                    two_theta = geometry.get_axe("two_theta").get_value();
                  }
                // eulerian4C::Vertical
                else if (type == typeid(geometry::eulerian4C::Vertical))
                  {
                    if (fabs(geometry.get_axe("chi").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("phi").get_value()) < constant::math::epsilon_1)
                      {
                        omega = geometry.get_axe("omega").get_value();
                        two_theta = geometry.get_axe("2theta").get_value();
                      }
                    else
                        throw HKLException("\"chi\" and/or \"phi\" axe(s) are wrong",
                                           "\"chi\" = \"phi\" must be set to zero",
                                           "geometry::twoC::Vertical::setFromGeometry(eulerian4C::Vertical const &)");
                  }
                // kappa4C::Vertical
                else if (type == typeid(geometry::kappa4C::Vertical))
                  {
                    if (fabs(geometry.get_axe("kappa").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("kphi").get_value()) < constant::math::epsilon_1)
                      {
                        omega = geometry.get_axe("komega").get_value();
                        two_theta = geometry.get_axe("2theta").get_value();
                      }
                    else
                        throw HKLException("\"kappa\" and/or \"kphi\" axe(s) are wrong",
                                           "\"kappa\" = \"kphi\" must be set to zero",
                                           "geometry::twoC::Vertical::setFromGeometry(kappa4C::Vertical const &)");
                  }
                // kappa6C
                else if (type == typeid(geometry::Kappa6C))
                  {
                    if (fabs(geometry.get_axe("gamma").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("mu").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("kappa").get_value()) < constant::math::epsilon_1
                        && fabs(geometry.get_axe("kphi").get_value()) < constant::math::epsilon_1)
                      {
                        omega = geometry.get_axe("komega").get_value();
                        two_theta = geometry.get_axe("delta").get_value();
                      }
                    else
                        throw HKLException("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" axe(s) are wrong",
                                           "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" must be set to zero",
                                           "geometry::kappa6C::Vertical::setFromGeometry(Kappa6C const &)");
                  }
                get_axe("omega").set_value(omega);
                get_axe("2theta").set_value(two_theta);
              }

        } // namespace twoC
    } // namespace geometry
} // namespace hkl
