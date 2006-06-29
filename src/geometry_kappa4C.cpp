#include <sstream>

#include "geometry_twoC.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "geometry_eulerian6C.h"
#include "constants.h"

namespace hkl {
    namespace geometry {
        namespace kappa4C {

            /*********************/
            /* Vertical Geometry */
            /*********************/
            Vertical::Vertical(void) :
              Kappa(50 * constant::math::degToRad)
            {
              m_komega = Axe("komega", svector(0., 1., 0.), -1);
              m_kappa = Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1);
              m_kphi = Axe("kphi", svector(0., 1., 0.), -1);
              m_tth = Axe("2theta", svector(0., 1., 0.), -1);

              addSampleAxe(m_komega);
              addSampleAxe(m_kappa);
              addSampleAxe(m_kphi);
              addDetectorAxe(m_tth);
            }

            Vertical::Vertical(double komega, double kappa, double kphi, double tth) :
              Kappa(50 * constant::math::degToRad)
            {
              m_komega = Axe("komega", svector(0., 1., 0.), -1, komega);
              m_kappa = Axe("kappa", svector(0., cos(m_alpha), sin(m_alpha)), -1, kappa);
              m_kphi = Axe("kphi", svector(0., 1., 0.), -1, kphi);
              m_tth = Axe("2theta", svector(0., 1., 0.), -1, tth);

              addSampleAxe(m_komega);
              addSampleAxe(m_kappa);
              addSampleAxe(m_kphi);
              addDetectorAxe(m_tth);
            }

            Vertical::Vertical(Vertical const & vertical) :
              Kappa(vertical),
              m_komega(vertical.m_komega),
              m_kappa(vertical.m_kappa),
              m_kphi(vertical.m_kphi),
              m_tth(vertical.m_tth)
            {
              addSampleAxe(m_komega);
              addSampleAxe(m_kappa);
              addSampleAxe(m_kphi);
              addDetectorAxe(m_tth);
            }

            Vertical::~Vertical(void)
              {}

            Vertical &
            Vertical::operator=(Vertical const & geometry)
              {
                Kappa::operator=(geometry);
                m_komega = geometry.m_komega;
                m_kappa = geometry.m_kappa;
                m_kphi = geometry.m_kphi;
                m_tth = geometry.m_tth;
                return *this;
              }

            void
            Vertical::setAngles(double const & komega, double const & kappa, double const & kphi, double const & two_theta)
              {
                m_komega.set_value(komega);
                m_kappa.set_value(kappa);
                m_kphi.set_value(kphi);
                m_tth.set_value(two_theta);
              }

            void
            Vertical::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                if (strict)
                  {
                    m_kappa.set_value(0);
                    m_kphi.set_value(0);
                  }
                m_komega.set_value(geometry.m_omega.get_value());
                m_tth.set_value(geometry.m_tth.get_value());
              }

            void
            Vertical::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                double const & chi = geometry.m_chi.get_value();
                if (fabs(chi) <= 2 * m_alpha)
                  {
                    double const & omega = geometry.m_omega.get_value();
                    double const & phi = geometry.m_phi.get_value();
                    double p = asin(tan(chi/2.)/tan(m_alpha));

                    m_komega.set_value(omega + p - constant::math::pi/2.);
                    m_kappa.set_value(-2 * asin(sin(chi/2.)/sin(m_alpha)));
                    m_kphi.set_value(phi + p + constant::math::pi/2.);
                    m_tth.set_value(geometry.m_tth.get_value());
                  }
                else
                  {
                    ostringstream description;
                    description << "\"chi\"(" << chi * constant::math::radToDeg << ") must be lower than " << 2*m_alpha*constant::math::radToDeg;
                    HKLEXCEPTION("\"chi\" is unreachable",
                                 description.str());
                  }
              }

            void
            Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                double const & mu = geometry.m_mu.get_value();
                double const & gamma = geometry.m_gamma.get_value();
                if ((!mu && !gamma) || !strict)
                  {
                    double const & chi = geometry.m_chi.get_value();
                    if (fabs(chi) <= 2 * m_alpha)
                      {
                        double const & omega = geometry.m_omega.get_value();
                        double const & phi = geometry.m_phi.get_value();
                        double p = asin(tan(chi/2.)/tan(m_alpha));

                        m_komega.set_value(omega + p - constant::math::pi/2.);
                        m_kappa.set_value(-2 * asin(sin(chi/2.)/sin(m_alpha)));
                        m_kphi.set_value(phi + p + constant::math::pi/2.);
                        m_tth.set_value(geometry.m_delta.get_value());
                      }
                    else
                      {
                        ostringstream description;
                        description << "\"chi\"(" << chi * constant::math::radToDeg << ") must be lower than " << 2*m_alpha*constant::math::radToDeg;
                        HKLEXCEPTION("\"chi\" is unreachable",
                                     description.str());
                      }
                  }
                else
                  {
                    ostringstream description;
                    if (mu && gamma)
                      {
                        description << "mu and gamma must be zero";
                      }
                    else if (mu)
                      {
                        description << "mu must be zero";
                      }
                    else if (gamma)
                      {
                        description << "gamma must be zero";
                      }
                    HKLEXCEPTION("Eulerian6C geometry is not compatible with a kappa4C::Vertical geometry.",
                                 description.str());
                  }
              }

            void
            Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                m_source = geometry.get_source();

                double const & mu = geometry.m_mu.get_value();
                double const & gamma = geometry.m_gamma.get_value();
                if ((!mu && !gamma) || !strict)
                  {
                    m_komega.set_value(geometry.m_komega.get_value());
                    m_kappa.set_value(geometry.m_kappa.get_value());
                    m_kphi.set_value(geometry.m_kphi.get_value());
                    m_tth.set_value(geometry.m_delta.get_value());
                  }
                else
                  {
                    ostringstream description;
                    if (mu && gamma)
                      {
                        description << "mu and gamma must be zero";
                      }
                    else if (mu)
                      {
                        description << "mu must be zero";
                      }
                    else if (gamma)
                      {
                        description << "gamma must be zero";
                      }
                    HKLEXCEPTION("kappa6C geometry is not compatible with a kappa4C::Vertical geometry.",
                                 description.str());
                  }
              }

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                Geometry::toStream(flux);
                m_komega.toStream(flux);
                m_kappa.toStream(flux);
                m_kphi.toStream(flux);
                m_tth.toStream(flux);
                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                Geometry::fromStream(flux);
                m_komega.fromStream(flux);
                m_kappa.fromStream(flux);
                m_kphi.fromStream(flux);
                m_tth.fromStream(flux);
                return flux;
              }

        } // namespace kappa4C
    } // namespace geometry
} // namespace hkl
