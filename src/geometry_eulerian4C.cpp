#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "pseudoaxe_eulerian4C.h"

namespace hkl {
    namespace geometry {
        namespace eulerian4C {

            Vertical::Vertical(void)
            : Geometry()
              {
                m_omega = Axe("omega", svector(0., 1., 0.), -1);
                m_chi = Axe("chi", svector(1., 0., 0.), 1);
                m_phi = Axe("phi", svector(0., 1., 0.), -1);
                m_tth = Axe("2theta", svector(0., 1., 0.), -1);

                addSampleAxe(m_omega);
                addSampleAxe(m_chi);
                addSampleAxe(m_phi);
                addDetectorAxe(m_tth);

                m_source.setDirection(svector(1,0,0));
              }

            Vertical::Vertical(Vertical const & geometry) :
              Geometry(geometry),
              m_omega(geometry.m_omega),
              m_chi(geometry.m_chi),
              m_phi(geometry.m_phi),
              m_tth(geometry.m_tth)
            {
              addSampleAxe(m_omega);
              addSampleAxe(m_chi);
              addSampleAxe(m_phi);
              addDetectorAxe(m_tth);
            }

            Vertical::Vertical(double const & omega, double const & chi, double const & phi, double const & two_theta) :
              Geometry()
            {
              m_omega = Axe("omega", svector(0., 1., 0.), -1, omega);
              m_chi = Axe("chi", svector(1., 0., 0.), 1, chi);
              m_phi = Axe("phi", svector(0., 1., 0.), -1, phi);
              m_tth = Axe("2theta", svector(0., 1., 0.), -1, two_theta);

              addSampleAxe(m_omega);
              addSampleAxe(m_chi);
              addSampleAxe(m_phi);
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
                m_chi = geometry.m_chi;
                m_phi = geometry.m_phi;
                m_tth = geometry.m_tth;
                return *this;
              }

            void
            Vertical::setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta)
              {
                m_omega.set_value(omega);
                m_chi.set_value(chi);
                m_phi.set_value(phi);
                m_tth.set_value(two_theta);
              }

            void
            Vertical::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if (strict)
                  {
                    m_chi.set_value(0);
                    m_phi.set_value(0);
                  }
                m_omega.set_value(geometry.m_omega.get_value());
                m_tth.set_value(geometry.m_tth.get_value());
              }

            void
            Vertical::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                double const & alpha = geometry.get_alpha();
                double const & komega = geometry.m_komega.get_value();
                double const & kappa = geometry.m_kappa.get_value();
                double const & kphi = geometry.m_kphi.get_value();

                m_omega.set_value(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
                m_chi.set_value(-2 * asin(sin(kappa/2.) * sin(alpha)));
                m_phi.set_value(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
                m_tth.set_value(geometry.m_tth.get_value());
              }

            void
            Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_gamma.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_mu.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    m_omega.set_value(geometry.m_omega.get_value());
                    m_chi.set_value(geometry.m_chi.get_value());
                    m_phi.set_value(geometry.m_phi.get_value());
                    m_tth.set_value(geometry.m_delta.get_value());
                  }
                else
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" axe(s) are wrong",
                                 "\"gamma\" = \"mu\" must be set to zero");
              }

            void
            Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if ((fabs(geometry.m_gamma.get_value()) < constant::math::epsilon_1
                     && fabs(geometry.m_mu.get_value()) < constant::math::epsilon_1) || !strict)
                  {
                    double const & alpha = geometry.get_alpha();
                    double const & komega = geometry.m_komega.get_value();
                    double const & kappa = geometry.m_kappa.get_value();
                    double const & kphi = geometry.m_kphi.get_value();

                    m_omega.set_value(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
                    m_chi.set_value(-2 * asin(sin(kappa/2.) * sin(alpha)));
                    m_phi.set_value(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
                    m_tth.set_value(geometry.m_delta.get_value());
                  }
                else
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" axe(s) are wrong",
                                 "\"gamma\" = \"mu\" must be set to zero");
              }

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                Geometry::toStream(flux);
                m_omega.toStream(flux);
                m_chi.toStream(flux);
                m_phi.toStream(flux);
                m_tth.toStream(flux);
                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                Geometry::fromStream(flux);
                m_omega.fromStream(flux);
                m_chi.fromStream(flux);
                m_phi.fromStream(flux);
                m_tth.fromStream(flux);
                return flux;
              }

            /**************
             * HORIZONTAL *
             **************/

            Horizontal::Horizontal(void)
            : Geometry()
              {
                m_omega = Axe("omega", svector(0., 0., 1.), 1);
                m_chi = Axe("chi", svector(1., 0., 0.), 1);
                m_phi = Axe("phi", svector(0., 1., 0.), -1);
                m_tth = Axe("2theta", svector(0., 1., 0.), -1);

                addSampleAxe(m_omega);
                addSampleAxe(m_chi);
                addSampleAxe(m_phi);
                addDetectorAxe(m_tth);
              }

            Horizontal::Horizontal(Horizontal const & geometry) :
              Geometry(geometry),
              m_omega(geometry.m_omega),
              m_chi(geometry.m_chi),
              m_phi(geometry.m_phi),
              m_tth(geometry.m_tth)
            {
              addSampleAxe(m_omega);
              addSampleAxe(m_chi);
              addSampleAxe(m_phi);
              addDetectorAxe(m_tth);
            }

            Horizontal::Horizontal(double omega, double chi, double phi, double two_theta)
            : Geometry()
              {
                m_omega = Axe("omega", svector(0., 0., 1.), 1, omega);
                m_chi = Axe("chi", svector(1., 0., 0.), 1, chi);
                m_phi = Axe("phi", svector(0., 1., 0.), -1, phi);
                m_tth = Axe("2theta", svector(0., 1., 0.), -1, two_theta);

                addSampleAxe(m_omega);
                addSampleAxe(m_chi);
                addSampleAxe(m_phi);
                addDetectorAxe(m_tth);
              }

            Horizontal::~Horizontal(void)
              {}

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl
