#include "geometry_eulerian6C.h"
#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {

        Eulerian6C::Eulerian6C(void) :
          Geometry()
          {
            m_mu = Axe("mu", svector(0., 0., 1.), 1);
            m_omega = Axe("omega", svector(0., 1., 0.), -1);
            m_chi = Axe("chi", svector(1., 0., 0.), 1);
            m_phi = Axe("phi", svector(0., 1., 0.), -1);
            m_gamma = Axe("gamma", svector(0., 0., 1.), 1);
            m_delta = Axe("delta", svector(0., 1., 0.), -1);

            addSampleAxe(m_mu);
            addSampleAxe(m_omega);
            addSampleAxe(m_chi);
            addSampleAxe(m_phi);

            addDetectorAxe(m_gamma);
            addDetectorAxe(m_delta);

            m_source.setDirection(svector(1,0,0));
          }

        Eulerian6C::Eulerian6C(Eulerian6C const & geometry) :
          Geometry(geometry),
          m_mu(geometry.m_mu),
          m_omega(geometry.m_omega),
          m_chi(geometry.m_chi),
          m_phi(geometry.m_phi),
          m_gamma(geometry.m_gamma),
          m_delta(geometry.m_delta)
        {
          addSampleAxe(m_mu);
          addSampleAxe(m_omega);
          addSampleAxe(m_chi);
          addSampleAxe(m_phi);

          addDetectorAxe(m_gamma);
          addDetectorAxe(m_delta);
        }

        Eulerian6C::Eulerian6C(double mu, double omega, double chi, double phi, double gamma, double delta)
          {
            m_mu = Axe("mu", svector(0., 0., 1.), 1, mu);
            m_omega = Axe("omega", svector(0., 1., 0.), -1, omega);
            m_chi = Axe("chi", svector(1., 0., 0.), 1, chi);
            m_phi = Axe("phi", svector(0., 1., 0.), -1, phi);
            m_gamma = Axe("gamma", svector(0., 0., 1.), 1, gamma);
            m_delta = Axe("delta", svector(0., 1., 0.), -1, delta);

            addSampleAxe(m_mu);
            addSampleAxe(m_omega);
            addSampleAxe(m_chi);
            addSampleAxe(m_phi);

            addDetectorAxe(m_gamma);
            addDetectorAxe(m_delta);

            m_source.setDirection(svector(1,0,0));
          }

        Eulerian6C::~Eulerian6C(void)
          {}

        Eulerian6C &
        Eulerian6C::operator=(Eulerian6C const & geometry)
          {
            Geometry::operator=(geometry);
            m_mu = geometry.m_mu;
            m_omega = geometry.m_omega;
            m_chi = geometry.m_chi;
            m_phi = geometry.m_phi;
            m_gamma = geometry.m_gamma;
            m_delta = geometry.m_delta;
            return *this;
          }

        void
        Eulerian6C::setAngles(double const & mu, double const & omega, double const & chi, double const & phi,
                              double const & gamma, double const & delta)
          {
            m_mu.set_value(mu);
            m_omega.set_value(omega);
            m_chi.set_value(chi);
            m_phi.set_value(phi);
            m_gamma.set_value(gamma);
            m_delta.set_value(delta);
          }

        void
        Eulerian6C::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            if (strict)
              {
                m_mu.set_value(0);
                m_chi.set_value(0);
                m_phi.set_value(0);
                m_gamma.set_value(0);
              }
            m_omega.set_value(geometry.m_omega.get_value());
            m_delta.set_value(geometry.m_tth.get_value());
          }

        void
        Eulerian6C::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            if (strict)
              {
                m_mu.set_value(0);
                m_gamma.set_value(0);
              }
            m_omega.set_value(geometry.m_omega.get_value());
            m_chi.set_value(geometry.m_chi.get_value());
            m_phi.set_value(geometry.m_phi.get_value());
            m_delta.set_value(geometry.m_tth.get_value());
          }

        void
        Eulerian6C::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            double const & alpha = geometry.get_alpha();
            double const & komega = geometry.m_komega.get_value();
            double const & kappa = geometry.m_kappa.get_value();
            double const & kphi = geometry.m_kphi.get_value();

            if (strict)
              {
                m_mu.set_value(0);
                m_gamma.set_value(0);
              }

            m_omega.set_value(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
            m_chi.set_value(-2 * asin(sin(kappa/2.) * sin(alpha)));
            m_phi.set_value(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
            m_delta.set_value(geometry.m_tth.get_value());
          }

        void
        Eulerian6C::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            double const & alpha = geometry.get_alpha();
            double const & komega = geometry.m_komega.get_value();
            double const & kappa = geometry.m_kappa.get_value();
            double const & kphi = geometry.m_kphi.get_value();

            m_mu.set_value(geometry.m_mu.get_value());
            m_omega.set_value(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
            m_chi.set_value(-2 * asin(sin(kappa/2.) * sin(alpha)));
            m_phi.set_value(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
            m_gamma.set_value(geometry.m_gamma.get_value());
            m_delta.set_value(geometry.m_delta.get_value());
          }

        ostream &
        Eulerian6C::toStream(ostream & flux) const
          {
            Geometry::toStream(flux);
            m_mu.toStream(flux);
            m_omega.toStream(flux);
            m_chi.toStream(flux);
            m_phi.toStream(flux);
            m_gamma.toStream(flux);
            m_delta.toStream(flux);
            return flux;
          }

        istream &
        Eulerian6C::fromStream(istream & flux)
          {
            Geometry::fromStream(flux);
            m_mu.fromStream(flux);
            m_omega.fromStream(flux);
            m_chi.fromStream(flux);
            m_phi.fromStream(flux);
            m_gamma.fromStream(flux);
            m_delta.fromStream(flux);
            return flux;
          }

    } // namespace geometry
} // namespace hkl
