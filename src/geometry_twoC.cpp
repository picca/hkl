#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {
        namespace twoC {

            Vertical::Vertical(void)
            : Geometry()
              {
                m_omega = Axe("omega", svector(0., 1., 0.), -1);
                m_tth = Axe("2theta", svector(0., 1., 0.), -1);
                addSampleAxe(m_omega);
                addDetectorAxe(m_tth);

                m_source.setDirection(svector(1,0,0));
              }

            Vertical::Vertical(Vertical const & geometry) :
              Geometry(geometry),
              m_omega(geometry.m_omega),
              m_tth(geometry.m_tth)
            {
              addSampleAxe(m_omega);
              addDetectorAxe(m_tth);
            }

            Vertical::Vertical(double const & omega, double const & two_theta)
            : Geometry()
              {
                m_omega = Axe("omega", svector(0., 1., 0.), -1, omega);
                m_tth = Axe("2theta", svector(0., 1., 0.), -1, two_theta);
                addSampleAxe(m_omega);
                addDetectorAxe(m_tth);

                m_source.setDirection(svector(1,0,0));
              }

            Vertical::~Vertical(void)
              {}

            Vertical &
            Vertical::operator=(Vertical const & geometry)
              {
                Geometry::operator=(geometry);
                m_omega = geometry.m_omega;
                m_tth = geometry.m_tth;
                return *this;
              }

            void
            Vertical::setAngles(double const & omega, double const & two_theta)
              {
                m_omega.set_value(omega);
                m_tth.set_value(two_theta);
              }

            void
            Vertical::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_chi.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_phi.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    m_omega.set_value(geometry.m_omega.get_value());
                    m_tth.set_value(geometry.m_tth.get_value());
                  }
                else
                    HKLEXCEPTION("\"chi\" and/or \"phi\" axe(s) are wrong",
                                 "\"chi\" = \"phi\" must be set to zero");
              }

            void
            Vertical::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_kappa.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_kphi.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    m_omega.set_value(geometry.m_komega.get_value());
                    m_tth.set_value(geometry.m_tth.get_value());
                  }
                else
                    HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) are wrong",
                                 "\"kappa\" = \"kphi\" must be set to zero");
              }

            void
            Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_gamma.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_mu.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_chi.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_phi.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    m_omega.set_value(geometry.m_omega.get_value());
                    m_tth.set_value(geometry.m_delta.get_value());
                  }
                else
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" axe(s) are wrong",
                                 "\"gamma\" = \"mu\" = \"chi\" = \"phi\" must be set to zero");
              }

            void
            Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_gamma.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_mu.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_kappa.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_kphi.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    m_omega.set_value(geometry.m_komega.get_value());
                    m_tth.set_value(geometry.m_delta.get_value());
                  }
                else
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" axe(s) are wrong",
                                 "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" must be set to zero");
              }

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                Geometry::toStream(flux);
                m_omega.toStream(flux);
                m_tth.toStream(flux);
                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                Geometry::fromStream(flux);
                m_omega.fromStream(flux);
                m_tth.fromStream(flux);
                return flux;
              }

        } // namespace twoC
    } // namespace geometry
} // namespace hkl
