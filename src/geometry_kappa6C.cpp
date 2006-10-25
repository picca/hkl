#include <sstream>

#include "geometry_twoC.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
#include "geometry_eulerian6C.h"

namespace hkl {
    namespace geometry {

        Kappa6C::Kappa6C(void) :
          Kappa("Kappa 6 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 6 circles diffractometer.", 50 * constant::math::degToRad)
        {
          _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
          _komega = addSampleAxe(Axe("komega", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
          _kappa = addSampleAxe(Axe("kappa", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
          _kphi = addSampleAxe(Axe("kphi", "4th sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
          _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
          _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));

          _source.setDirection(svector(1,0,0));
        }

        Kappa6C::Kappa6C(Kappa6C const & geometry) :
          Kappa(geometry)
        {
          _mu = &_axes["mu"];
          _komega = &_axes["komega"];
          _kappa = &_axes["kappa"];
          _kphi = &_axes["kphi"];
          _gamma = &_axes["gamma"];
          _delta = &_axes["delta"];
        }

        Kappa6C::Kappa6C(double mu, double komega, double kappa, double kphi, double gamma, double delta) :
          Kappa("Kappa 6 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 6 circles diffractometer.", 50 * constant::math::degToRad)
        {
          _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, mu, constant::math::pi, svector(0., 0., 1.), 1));
          _komega = addSampleAxe(Axe("komega", "2nd sample axe", -constant::math::pi, komega, constant::math::pi, svector(0., 1., 0.), -1));
          _kappa = addSampleAxe(Axe("kappa", "3rd sample axe", -constant::math::pi, kappa, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
          _kphi = addSampleAxe(Axe("kphi", "4th sample axe", -constant::math::pi, kphi, constant::math::pi, svector(0., 1., 0.), -1));
          _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, gamma, constant::math::pi, svector(0., 0., 1.), 1));
          _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, delta, constant::math::pi, svector(0., 1., 0.), -1));

          _source.setDirection(svector(1,0,0));
        }

        Kappa6C::~Kappa6C(void)
          {}

        Kappa6C &
        Kappa6C::operator=(Kappa6C const & geometry)
          {
            Kappa::operator=(geometry);
            _mu = &_axes["mu"];
            _komega = &_axes["komega"];
            _kappa = &_axes["kappa"];
            _kphi = &_axes["kphi"];
            _gamma = &_axes["gamma"];
            _delta = &_axes["delta"];
            return *this;
          }

        void
        Kappa6C::setAngles(double const & mu, double const & komega, double const & kappa, double const & kphi,
                           double const & gamma, double const & delta)
          {
            _mu->set_current(mu);
            _komega->set_current(komega);
            _kappa->set_current(kappa);
            _kphi->set_current(kphi);
            _gamma->set_current(gamma);
            _delta->set_current(delta);
          }

        void
        Kappa6C::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            if (strict)
              {
                _mu->set_current(0);
                _gamma->set_current(0);
                _kappa->set_current(0);
                _kphi->set_current(0);
              }
          }

        void
        Kappa6C::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            double const & chi = geometry._chi->get_current().get_value();
            if (fabs(chi) <= 2 * _alpha)
              {
                double const & omega = geometry._omega->get_current().get_value();
                double const & phi = geometry._phi->get_current().get_value();
                double p = asin(tan(chi/2.)/tan(_alpha));

                if (strict)
                  {
                    _mu->set_current(0);
                    _gamma->set_current(0);
                  }
                _komega->set_current(omega + p - constant::math::pi/2.);
                _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
                _kphi->set_current(phi + p + constant::math::pi/2.);
                _delta->set_current(geometry._tth->get_current());
              }
            else
              {
                ostringstream description;
                description << "The current E4CV \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
                HKLEXCEPTION("Can not convert geometry E4CV -> K6C",
                             description.str());
              }
          }

        void
        Kappa6C::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            if (strict)
              {
                _mu->set_current(0);
                _gamma->set_current(0);
              }
            _komega->set_current(geometry._komega->get_current());
            _kappa->set_current(geometry._kappa->get_current());
            _kphi->set_current(geometry._kphi->get_current());
            _delta->set_current(geometry._tth->get_current());
          }

        void
        Kappa6C::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            double const & chi = geometry._chi->get_current().get_value();
            if (chi <= 2 * _alpha)
              {
                double const & omega = geometry._omega->get_current().get_value();
                double const & phi = geometry._phi->get_current().get_value();
                double p = asin(tan(chi/2.)/tan(_alpha));

                _mu->set_current(geometry._mu->get_current());
                _komega->set_current(omega + p - constant::math::pi/2.);
                _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
                _kphi->set_current(phi + p + constant::math::pi/2.);
                _gamma->set_current(geometry._gamma->get_current());
                _delta->set_current(geometry._delta->get_current());
              }
            else
              {
                ostringstream description;
                description << "The current E6C \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
                HKLEXCEPTION("Can not convert geometry E6C -> K6C",
                             description.str());
              }
          }

    } // namespace geometry
} // namespace hkl
