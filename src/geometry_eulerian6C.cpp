#include "geometry_eulerian6C.h"
#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace geometry {

        Eulerian6C::Eulerian6C(void) :
          Geometry("Eulerian 6 circles", "A default Eulerian 6 circles diffractometer.")
        {
          _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
          _omega = addSampleAxe(Axe("omega", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
          _chi = addSampleAxe(Axe("chi", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(1., 0., 0.), 1));
          _phi = addSampleAxe(Axe("phi", "4th sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
          _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
          _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));

          _source.setDirection(svector(1,0,0));
        }

        Eulerian6C::Eulerian6C(Eulerian6C const & geometry) :
          Geometry(geometry)
        {
          _mu = &_axes["mu"];
          _omega = &_axes["omega"];
          _chi = &_axes["chi"];
          _phi = &_axes["phi"];
          _gamma = &_axes["gamma"];
          _delta = &_axes["delta"];
        }

        Eulerian6C::Eulerian6C(double mu, double omega, double chi, double phi, double gamma, double delta) :
          Geometry("Eulerian 6 circles", "A default Eulerian 6 circles diffractometer.")
        {
          _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, mu, constant::math::pi, svector(0., 0., 1.), 1));
          _omega = addSampleAxe(Axe("omega", "2nd sample axe", -constant::math::pi, omega, constant::math::pi, svector(0., 1., 0.), -1));
          _chi = addSampleAxe(Axe("chi", "3rd sample axe", -constant::math::pi, chi, constant::math::pi, svector(1., 0., 0.), 1));
          _phi = addSampleAxe(Axe("phi", "4th sample axe", -constant::math::pi, phi, constant::math::pi, svector(0., 1., 0.), -1));
          _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, gamma, constant::math::pi, svector(0., 0., 1.), 1));
          _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, delta, constant::math::pi, svector(0., 1., 0.), -1));

          _source.setDirection(svector(1,0,0));
        }

        Eulerian6C::~Eulerian6C(void)
          {}

        Eulerian6C &
        Eulerian6C::operator=(Eulerian6C const & geometry)
          {
            Geometry::operator=(geometry);
            _mu = &_axes["mu"];
            _omega = &_axes["omega"];
            _chi = &_axes["chi"];
            _phi = &_axes["phi"];
            _gamma = &_axes["gamma"];
            _delta = &_axes["delta"];
            return *this;
          }

        void
        Eulerian6C::setAngles(double const & mu, double const & omega, double const & chi, double const & phi,
                              double const & gamma, double const & delta)
          {
            _mu->set_current(mu);
            _omega->set_current(omega);
            _chi->set_current(chi);
            _phi->set_current(phi);
            _gamma->set_current(gamma);
            _delta->set_current(delta);
          }

        void
        Eulerian6C::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            if (strict)
              {
                _mu->set_current(0);
                _chi->set_current(0);
                _phi->set_current(0);
                _gamma->set_current(0);
              }
            _omega->set_current(geometry._omega->get_current());
            _delta->set_current(geometry._tth->get_current());
          }

        void
        Eulerian6C::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            if (strict)
              {
                _mu->set_current(0);
                _gamma->set_current(0);
              }
            _omega->set_current(geometry._omega->get_current());
            _chi->set_current(geometry._chi->get_current());
            _phi->set_current(geometry._phi->get_current());
            _delta->set_current(geometry._tth->get_current());
          }

        void
        Eulerian6C::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            double const & alpha = geometry.get_alpha();
            double const & komega = geometry._komega->get_current().get_value();
            double const & kappa = geometry._kappa->get_current().get_value();
            double const & kphi = geometry._kphi->get_current().get_value();

            if (strict)
              {
                _mu->set_current(0);
                _gamma->set_current(0);
              }

            _omega->set_current(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
            _chi->set_current(-2 * asin(sin(kappa/2.) * sin(alpha)));
            _phi->set_current(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
            _delta->set_current(geometry._tth->get_current());
          }

        void
        Eulerian6C::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
          {
            // update the source
            _source = geometry._source;

            double const & alpha = geometry.get_alpha();
            double const & komega = geometry._komega->get_current().get_value();
            double const & kappa = geometry._kappa->get_current().get_value();
            double const & kphi = geometry._kphi->get_current().get_value();

            _mu->set_current(geometry._mu->get_current());
            _omega->set_current(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
            _chi->set_current(-2 * asin(sin(kappa/2.) * sin(alpha)));
            _phi->set_current(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
            _gamma->set_current(geometry._gamma->get_current());
            _delta->set_current(geometry._delta->get_current());
          }

    } // namespace geometry
} // namespace hkl
