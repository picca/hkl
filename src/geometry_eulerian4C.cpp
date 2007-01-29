#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"
//#include "pseudoaxe_eulerian4C.h"

namespace hkl
  {
  namespace geometry
    {
    namespace eulerian4C
      {

      Vertical::Vertical(void)
          : Geometry("Eulerian 4 circles", "The LPS (Orsay) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        _omega = addSampleAxe(Axe("omega", "1st sample axe", -180 * constant::math::degToRad, 0, 180 * constant::math::degToRad, svector(0., 1., 0.), -1));
        _chi = addSampleAxe(Axe("chi", "2nd sample axe", -180 * constant::math::degToRad, 0, 180 * constant::math::degToRad, svector(1., 0., 0.), 1));
        _phi = addSampleAxe(Axe("phi", "3rd sample axe", -180 * constant::math::degToRad, 0, 180 * constant::math::degToRad, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "Detector axe", -180 * constant::math::degToRad, 0, 180 * constant::math::degToRad, svector(0., 1., 0.), -1));
      }

      Vertical::Vertical(Vertical const & geometry) :
          Geometry(geometry)
      {
        _omega = &(_axes["omega"]);
        _chi = &(_axes["chi"]);
        _phi = &(_axes["phi"]);
        _tth = &(_axes["2theta"]);
      }

      Vertical::Vertical(double const & omega, double const & chi, double const & phi, double const & two_theta) :
          Geometry("Eulerian 4 circles", "The LPS (Orsay) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        _omega = addSampleAxe(Axe("omega", "1st sample axe", -constant::math::pi, omega, constant::math::pi, svector(0., 1., 0.), -1));
        _chi = addSampleAxe(Axe("chi", "2nd sample axe", -constant::math::pi, chi, constant::math::pi, svector(1., 0., 0.), 1));
        _phi = addSampleAxe(Axe("phi", "3rd sample axe", -constant::math::pi, phi, constant::math::pi, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "Detector axe", -constant::math::pi, two_theta, constant::math::pi, svector(0., 1., 0.), -1));
      }

      Vertical::~Vertical(void)
      {}

      Vertical &
      Vertical::operator=(Vertical const & vertical)
      {
        Geometry::operator=(vertical);

        _omega = &(_axes["omega"]);
        _chi = &(_axes["chi"]);
        _phi = &(_axes["phi"]);
        _tth = &(_axes["2theta"]);

        return *this;
      }

      void
      Vertical::setAngles(double const & omega, double const & chi, double const & phi, double const & two_theta)
      {
        _omega->set_current(omega);
        _chi->set_current(chi);
        _phi->set_current(phi);
        _tth->set_current(two_theta);
      }

      void
      Vertical::setFromGeometry(geometry::twoC::Vertical const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if (strict)
          {
            _chi->set_current(0);
            _phi->set_current(0);
          }
        _omega->set_current(geometry.omega()->get_current().get_value());
        _tth->set_current(geometry.tth()->get_current().get_value());
      }

      void
      Vertical::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        double const & alpha = geometry.get_alpha();
        double const & komega = geometry.komega()->get_current().get_value();
        double const & kappa = geometry.kappa()->get_current().get_value();
        double const & kphi = geometry.kphi()->get_current().get_value();
        double p = atan(tan(kappa/2.) * cos(alpha));

        _omega->set_current(komega + p + constant::math::pi/2.);
        _chi->set_current(-2 * asin(sin(kappa/2.) * sin(alpha)));
        _phi->set_current(kphi + p - constant::math::pi/2.);
        _tth->set_current(geometry.tth()->get_current());
      }

      void
      Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon
             && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon) || !strict)
          {
            _omega->set_current(geometry.omega()->get_current());
            _chi->set_current(geometry.chi()->get_current());
            _phi->set_current(geometry.phi()->get_current());
            _tth->set_current(geometry.delta()->get_current());
          }
        else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" must be set to zero");
      }

      void
      Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon
             && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon) || !strict)
          {
            double const & alpha = geometry.get_alpha();
            double const & komega = geometry.komega()->get_current().get_value();
            double const & kappa = geometry.kappa()->get_current().get_value();
            double const & kphi = geometry.kphi()->get_current().get_value();

            _omega->set_current(komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.);
            _chi->set_current(-2 * asin(sin(kappa/2.) * sin(alpha)));
            _phi->set_current(kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.);
            _tth->set_current(geometry.delta()->get_current());
          }
        else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" must be set to zero");
      }

      /**************
       * HORIZONTAL *
       **************/

      Horizontal::Horizontal(void)
          : Geometry("Eulerian 4 Circles Horizontal", "test")
      {
        _omega = addSampleAxe(Axe("omega", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
        _chi = addSampleAxe(Axe("chi", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(1., 0., 0.), 1));
        _phi = addSampleAxe(Axe("phi", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
      }

      Horizontal::Horizontal(Horizontal const & geometry) :
          Geometry(geometry)
      {
        _omega = &_axes["omega"];
        _chi = &_axes["chi"];
        _phi = &_axes["phi"];
        _tth = &_axes["2theta"];
      }

      Horizontal::Horizontal(double omega, double chi, double phi, double two_theta)
          : Geometry("Eulerian 4 Circles Horizontal", "test")
      {
        _omega = addSampleAxe(Axe("omega", "1st sample axe", -constant::math::pi, omega, constant::math::pi, svector(0., 0., 1.), 1));
        _chi = addSampleAxe(Axe("chi", "2nd sample axe", -constant::math::pi, chi, constant::math::pi, svector(1., 0., 0.), 1));
        _phi = addSampleAxe(Axe("phi", "3rd sample axe", -constant::math::pi, phi, constant::math::pi, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "detector axe", -constant::math::pi, two_theta, constant::math::pi, svector(0., 1., 0.), -1));
      }

      Horizontal::~Horizontal(void)
      {}

    } // namespace eulerian4C
  } // namespace geometry
} // namespace hkl
