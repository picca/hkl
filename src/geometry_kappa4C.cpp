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
              Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", 50 * constant::math::degToRad)
            {
              _komega = addSampleAxe(Axe("komega", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
              _kappa = addSampleAxe(Axe("kappa", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
              _kphi = addSampleAxe(Axe("kphi", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
              _tth = addDetectorAxe(Axe("2theta", "1st detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));

              _source.setDirection(svector(1,0,0));
            }

            Vertical::Vertical(double komega, double kappa, double kphi, double tth) :
              Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", 50 * constant::math::degToRad)
            {
              _komega = addSampleAxe(Axe("komega", "1st sample axe", -constant::math::pi, komega, constant::math::pi, svector(0., 1., 0.), -1));
              _kappa = addSampleAxe(Axe("kappa", "2nd sample axe", -constant::math::pi, kappa, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
              _kphi = addSampleAxe(Axe("kphi", "3rd sample axe", -constant::math::pi, kphi, constant::math::pi, svector(0., 1., 0.), -1));
              _tth = addDetectorAxe(Axe("2theta", "1st detector axe", -constant::math::pi, tth, constant::math::pi, svector(0., 1., 0.), -1));

              _source.setDirection(svector(1,0,0));
            }

            Vertical::Vertical(Vertical const & vertical) :
              Kappa(vertical)
            {
              _komega = &_axes["komega"];
              _kappa = &_axes["kappa"];
              _kphi = &_axes["kphi"];
              _tth = &_axes["2theta"];
            }

            Vertical::~Vertical(void)
              {}

            Vertical &
            Vertical::operator=(Vertical const & geometry)
              {
                Kappa::operator=(geometry);
                _komega = &_axes["komega"];
                _kappa = &_axes["kappa"];
                _kphi = &_axes["kphi"];
                _tth = &_axes["2theta"];
                return *this;
              }

            void
            Vertical::setAngles(double const & komega, double const & kappa, double const & kphi, double const & two_theta)
              {
                _komega->set_current(komega);
                _kappa->set_current(kappa);
                _kphi->set_current(kphi);
                _tth->set_current(two_theta);
              }

            void
            Vertical::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                _source = geometry._source;

                if (strict)
                  {
                    _kappa->set_current(0);
                    _kphi->set_current(0);
                  }
                _komega->set_current(geometry._omega->get_current());
                _tth->set_current(geometry._tth->get_current());
              }

            void
            Vertical::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                _source = geometry._source;

                double const & chi = geometry._chi->get_current().get_value();
                if (fabs(chi) <= 2 * _alpha)
                  {
                    double const & omega = geometry._omega->get_current().get_value();
                    double const & phi = geometry._phi->get_current().get_value();
                    double p = asin(tan(chi/2.)/tan(_alpha));

                    _komega->set_current(omega + p - constant::math::pi/2.);
                    _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
                    _kphi->set_current(phi + p + constant::math::pi/2.);
                    _tth->set_current(geometry._tth->get_current());
                  }
                else
                  {
                    ostringstream description;
                    description << "The current E4CV \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
                    HKLEXCEPTION("Can not convert geometry E4CV -> K4CV",
                                 description.str());
                  }
              }

            void
            Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                _source = geometry._source;

                double const & mu = geometry._mu->get_current().get_value();
                double const & gamma = geometry._gamma->get_current().get_value();
                if ((!mu && !gamma) || !strict)
                  {
                    double const & chi = geometry._chi->get_current().get_value();
                    if (fabs(chi) <= 2 * _alpha)
                      {
                        double const & omega = geometry._omega->get_current().get_value();
                        double const & phi = geometry._phi->get_current().get_value();
                        double p = asin(tan(chi/2.)/tan(_alpha));

                        _komega->set_current(omega + p - constant::math::pi/2.);
                        _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
                        _kphi->set_current(phi + p + constant::math::pi/2.);
                        _tth->set_current(geometry._delta->get_current());
                      }
                    else
                      {
                        ostringstream description;
                        description << "The current E6C \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
                        HKLEXCEPTION("Can not convert geometry E6C -> K4CV",
                                     description.str());
                      }
                  }
                else
                  {
                    ostringstream description;
                    if (mu && gamma)
                      {
                        description << "the current E6C \"mu\" (" << mu * constant::math::radToDeg << "°) and \"gamma\" (" << gamma * constant::math::radToDeg << "°) axes must be set to zero";
                      }
                    else if (mu)
                      {
                        description << "the current E6C \"mu\" (" << mu * constant::math::radToDeg << "°) must be set to zero";
                      }
                    else if (gamma)
                      {
                        description << "the current E6C \"gamma\" (" << gamma * constant::math::radToDeg << "°) must be set to zero";
                      }
                    HKLEXCEPTION("Can not convert geometry E6C -> K4CV",
                                 description.str());
                  }
              }

            void
            Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
              {
                // update the source
                _source = geometry._source;

                double const & mu = geometry._mu->get_current().get_value();
                double const & gamma = geometry._gamma->get_current().get_value();
                if ((!mu && !gamma) || !strict)
                  {
                    _komega->set_current(geometry._komega->get_current());
                    _kappa->set_current(geometry._kappa->get_current());
                    _kphi->set_current(geometry._kphi->get_current());
                    _tth->set_current(geometry._delta->get_current());
                  }
                else
                  {
                    ostringstream description;
                    if (mu && gamma)
                      {
                        description << "the current K6C \"mu\" (" << mu * constant::math::radToDeg << "°) and \"gamma\" (" << gamma * constant::math::radToDeg << "°) axes must be set to zero";
                      }
                    else if (mu)
                      {
                        description << "the current K6C \"mu\" (" << mu * constant::math::radToDeg << "°) must be set to zero";
                      }
                    else if (gamma)
                      {
                        description << "the current K6C \"gamma\" (" << gamma * constant::math::radToDeg << "°) must be set to zero";
                      }
                    HKLEXCEPTION("Can not convert geometry K6C -> K4CV",
                                 description.str());
                  }
              }

        } // namespace kappa4C
    } // namespace geometry
} // namespace hkl
