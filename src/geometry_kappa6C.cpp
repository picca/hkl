#include <sstream>

#include "geometry_twoC.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "geometry_eulerian6C.h"

namespace hkl {
    namespace geometry {

        Kappa6C::Kappa6C(void) :
          Kappa(50 * constant::math::degToRad)
        {
          m_mu = Axe("mu", svector(0., 0., 1.), 1);
          m_komega = Axe("komega", svector(0., 1., 0.), -1);
          m_kappa = Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1);
          m_kphi = Axe("kphi", svector(0., 1., 0.), -1);
          m_gamma = Axe("gamma", svector(0., 0., 1.), 1);
          m_delta = Axe("delta", svector(0., 1., 0.), -1);

          addSampleAxe(m_mu);
          addSampleAxe(m_komega);
          addSampleAxe(m_kappa);
          addSampleAxe(m_kphi);

          addDetectorAxe(m_gamma);
          addDetectorAxe(m_delta);
        }

        Kappa6C::Kappa6C(Kappa6C const & geometry) :
          Kappa(geometry),
          m_mu(geometry.m_mu),
          m_komega(geometry.m_komega),
          m_kappa(geometry.m_kappa),
          m_kphi(geometry.m_kphi),
          m_gamma(geometry.m_gamma),
          m_delta(geometry.m_delta)
        {
          addSampleAxe(m_mu);
          addSampleAxe(m_komega);
          addSampleAxe(m_kappa);
          addSampleAxe(m_kphi);

          addDetectorAxe(m_gamma);
          addDetectorAxe(m_delta);
        }

        Kappa6C::Kappa6C(double mu, double komega, double kappa, double kphi, double gamma, double delta) :
          Kappa(50 * constant::math::degToRad)
        {
          m_mu = Axe("mu", svector(0., 0., 1.), 1, mu);
          m_komega = Axe("komega", svector(0., 1., 0.), -1, komega);
          m_kappa = Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa);
          m_kphi = Axe("kphi", svector(0., 1., 0.), -1, kphi);
          m_gamma = Axe("gamma", svector(0., 0., 1.), 1, gamma);
          m_delta = Axe("delta", svector(0., 1., 0.), -1, delta);

          addSampleAxe(m_mu);
          addSampleAxe(m_komega);
          addSampleAxe(m_kappa);
          addSampleAxe(m_kphi);

          addDetectorAxe(m_gamma);
          addDetectorAxe(m_delta);
        }

        Kappa6C::~Kappa6C(void)
          {}

        Kappa6C &
        Kappa6C::operator=(Kappa6C const & geometry)
          {
            Kappa::operator=(geometry);
            m_mu = geometry.m_mu;
            m_komega = geometry.m_komega;
            m_kappa = geometry.m_kappa;
            m_kphi = geometry.m_kphi;
            m_gamma = geometry.m_gamma;
            m_delta = geometry.m_delta;
            return *this;
          }

        void
        Kappa6C::setAngles(double const & mu, double const & komega, double const & kappa, double const & kphi,
                           double const & gamma, double const & delta)
          {
            m_mu.set_value(mu);
            m_komega.set_value(komega);
            m_kappa.set_value(kappa);
            m_kphi.set_value(kphi);
            m_gamma.set_value(gamma);
            m_delta.set_value(delta);
          }

        void
        Kappa6C::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            if (strict)
              {
                m_mu.set_value(0);
                m_gamma.set_value(0);
                m_kappa.set_value(0);
                m_kphi.set_value(0);
              }
          }

        void
        Kappa6C::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            double const & chi = geometry.m_chi.get_value();
            if (fabs(chi) <= 2 * m_alpha)
              {
                double const & omega = geometry.m_omega.get_value();
                double const & phi = geometry.m_phi.get_value();
                double p = asin(tan(chi/2.)/tan(m_alpha));

                if (strict)
                  {
                    m_mu.set_value(0);
                    m_gamma.set_value(0);
                  }
                m_komega.set_value(omega + p - constant::math::pi/2.);
                m_kappa.set_value(-2 * asin(sin(chi/2.)/sin(m_alpha)));
                m_kphi.set_value(phi + p + constant::math::pi/2.);
                m_delta.set_value(geometry.m_tth.get_value());
              }
            else
              {
                ostringstream description;
                description << "\"chi\" must be lower than " << 2*m_alpha*constant::math::radToDeg;
                HKLEXCEPTION("\"chi\" is unreachable",
                             description.str());
              }
          }

        void
        Kappa6C::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            if (strict)
              {
                m_mu.set_value(0);
                m_gamma.set_value(0);
              }
            m_komega.set_value(geometry.m_komega.get_value());
            m_kappa.set_value(geometry.m_kappa.get_value());
            m_kphi.set_value(geometry.m_kphi.get_value());
            m_delta.set_value(geometry.m_tth.get_value());
          }

        void
        Kappa6C::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            m_source = geometry.get_source();

            double const & chi = geometry.m_chi.get_value();
            if (chi <= 2 * m_alpha)
              {
                double const & omega = geometry.get_axe("omega").get_value();
                double const & phi = geometry.get_axe("phi").get_value();
                double p = asin(tan(chi/2.)/tan(m_alpha));

                m_mu.set_value(geometry.m_mu.get_value());
                m_komega.set_value(omega + p - constant::math::pi/2.);
                m_kappa.set_value(-2 * asin(sin(chi/2.)/sin(m_alpha)));
                m_kphi.set_value(phi + p + constant::math::pi/2.);
                m_gamma.set_value(geometry.m_gamma.get_value());
                m_delta.set_value(geometry.m_delta.get_value());
              }
            else
              {
                ostringstream description;
                description << "\"chi\" must be lower than " << 2*m_alpha*constant::math::radToDeg;
                HKLEXCEPTION("\"chi\" is unreachable",
                             description.str());
              }
          }

        ostream &
        Kappa6C::toStream(ostream & flux) const
          {
            Geometry::toStream(flux);
            m_mu.toStream(flux);
            m_komega.toStream(flux);
            m_kappa.toStream(flux);
            m_kphi.toStream(flux);
            m_gamma.toStream(flux);
            m_delta.toStream(flux);
            return flux;
          }

        istream &
        Kappa6C::fromStream(istream & flux)
          {
            Geometry::fromStream(flux);
            m_mu.fromStream(flux);
            m_komega.fromStream(flux);
            m_kappa.fromStream(flux);
            m_kphi.fromStream(flux);
            m_gamma.fromStream(flux);
            m_delta.fromStream(flux);
            return flux;
          }

    } // namespace geometry
} // namespace hkl
