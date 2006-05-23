#include "geometry_eulerian6C.h"
#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {

        Eulerian6C::Eulerian6C(void)
          {
            addSampleAxe(Axe("mu", svector(0., 0., 1.), 1));
            addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
            addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
            addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

            addDetectorAxe(Axe("gamma", svector(0., 0., 1.), 1));
            addDetectorAxe(Axe("delta", svector(0., 1., 0.), -1));
          }

        Eulerian6C::Eulerian6C(double mu, double omega, double chi, double phi, double gamma, double delta)
          {
            addSampleAxe(Axe("mu", svector(0., 0., 1.), 1, mu));
            addSampleAxe(Axe("omega", svector(0., 1., 0.), -1, omega));
            addSampleAxe(Axe("chi", svector(1., 0., 0.), 1, chi));
            addSampleAxe(Axe("phi", svector(0., 1., 0.), -1, phi));

            addDetectorAxe(Axe("gamma", svector(0., 0., 1.), 1, gamma));
            addDetectorAxe(Axe("delta", svector(0., 1., 0.), -1, delta));
          }

        Eulerian6C::~Eulerian6C(void)
          {}

        void
        Eulerian6C::setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException)
          {
            Axe & Mu = get_axe("mu");
            Axe & Omega = get_axe("omega");
            Axe & Chi = get_axe("chi");
            Axe & Phi = get_axe("phi");
            Axe & Gamma = get_axe("gamma");
            Axe & Delta = get_axe("delta");
            double mu;
            double omega;
            double chi;
            double phi;
            double gamma;
            double delta;

            // update the source
            m_source = geometry.get_source();

            const type_info & type = typeid(geometry);
            // twoC::Vertical
            if (type == typeid(geometry::twoC::Vertical))
              {
                omega = geometry.get_axe("omega").get_value();
                delta = geometry.get_axe("2theta").get_value();
                if (strict)
                    mu = chi = phi = gamma = 0;
                else
                  {
                    mu = Mu.get_value();
                    chi = Chi.get_value();
                    phi = Phi.get_value();
                    gamma = Gamma.get_value();
                  }
              }
            // eulerian4C::Vertical
            else if (type == typeid(geometry::eulerian4C::Vertical))
              {
                omega = geometry.get_axe("omega").get_value();
                chi = geometry.get_axe("chi").get_value();
                phi = geometry.get_axe("phi").get_value();
                delta = geometry.get_axe("2theta").get_value();
                if (strict)
                    mu = gamma = 0;
                else
                  {
                    mu = Mu.get_value();
                    gamma = Gamma.get_value();
                  }
              }
            // kappa4C::Vertical
            else if (type == typeid(geometry::kappa4C::Vertical))
              {
                double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                double const & komega = geometry.get_axe("komega").get_value();
                double const & kappa = geometry.get_axe("kappa").get_value();
                double const & kphi = geometry.get_axe("kphi").get_value();

                omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                delta = geometry.get_axe("2theta").get_value();
                if (strict)
                    mu = gamma = 0;
                else
                  {
                    mu = Mu.get_value();
                    gamma = Gamma.get_value();
                  }
              }
            // kappa6C
            else if (type == typeid(geometry::Kappa6C))
              {
                double const & alpha = static_cast<geometry::kappa4C::Vertical const &>(geometry).get_alpha();
                double const & komega = geometry.get_axe("komega").get_value();
                double const & kappa = geometry.get_axe("kappa").get_value();
                double const & kphi = geometry.get_axe("kphi").get_value();

                mu = geometry.get_axe("mu").get_value();
                omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                gamma = geometry.get_axe("gamma").get_value();
                delta = geometry.get_axe("delta").get_value();
              }
            else
                HKLEXCEPTION("updating the Eulerian6C geometry from this kind of geometry is not possible.",
                             "write the convertion method ;)");

            Mu.set_value(mu);
            Omega.set_value(omega);
            Chi.set_value(chi);
            Phi.set_value(phi);
            Gamma.set_value(gamma);
            Delta.set_value(delta);
          }

    } // namespace geometry
} // namespace hkl
