#include "geometry_twoC.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"
#include "geometry_kappa4C.h"
#include "geometry_kappa6C.h"

namespace hkl
  {
  namespace geometry
    {
    namespace twoC
      {

      Vertical::Vertical(void)
          : Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
      {
        _omega = addSampleAxe(Axe("omega", "The sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "The detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));

        _source.setDirection(svector(1,0,0));
      }

      Vertical::Vertical(Vertical const & geometry) :
          Geometry(geometry)
      {
        _omega = &_axes["omega"];
        _tth = &_axes["2theta"];
      }

      Vertical::Vertical(double const & omega, double const & two_theta)
          : Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
      {
        _omega = addSampleAxe(Axe("omega", "The sample axe", -constant::math::pi, omega, constant::math::pi, svector(0., 1., 0.), -1));
        _tth = addDetectorAxe(Axe("2theta", "The detector axe", -constant::math::pi, two_theta, constant::math::pi, svector(0., 1., 0.), -1));

        _source.setDirection(svector(1,0,0));
      }

      Vertical::~Vertical(void)
      {}

      Vertical &
      Vertical::operator=(Vertical const & geometry)
      {
        Geometry::operator=(geometry);
        _omega = &_axes["omega"];
        _tth = &_axes["2theta"];
        return *this;
      }

      void
      Vertical::setAngles(double const & omega, double const & two_theta)
      {
        _omega->set_current(omega);
        _tth->set_current(two_theta);
      }

      void
      Vertical::setFromGeometry(geometry::eulerian4C::Vertical const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.chi()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.phi()->get_current().get_value()) < constant::math::epsilon_1) || !strict)
          {
            _omega->set_current(geometry.omega()->get_current().get_value());
            _tth->set_current(geometry.tth()->get_current().get_value());
          }
        else
          HKLEXCEPTION("\"chi\" and/or \"phi\" axe(s) are wrong",
                       "\"chi\" = \"phi\" must be set to zero");
      }

      void
      Vertical::setFromGeometry(geometry::kappa4C::Vertical const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.kappa()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.kphi()->get_current().get_value()) < constant::math::epsilon_1) || !strict)
          {
            _omega->set_current(geometry.komega()->get_current().get_value());
            _tth->set_current(geometry.tth()->get_current().get_value());
          }
        else
          HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) are wrong",
                       "\"kappa\" = \"kphi\" must be set to zero");
      }

      void
      Vertical::setFromGeometry(geometry::Eulerian6C const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.chi()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.phi()->get_current().get_value()) < constant::math::epsilon_1) || !strict)
          {
            _omega->set_current(geometry.omega()->get_current());
            _tth->set_current(geometry.delta()->get_current());
          }
        else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" = \"chi\" = \"phi\" must be set to zero");
      }

      void
      Vertical::setFromGeometry(geometry::Kappa6C const & geometry, bool const & strict) throw (HKLException)
      {
        // update the source
        _source = geometry.get_source();

        if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.kappa()->get_current().get_value()) < constant::math::epsilon_1
             && fabs(geometry.kphi()->get_current().get_value()) < constant::math::epsilon_1) || !strict)
          {
            _omega->set_current(geometry.komega()->get_current());
            _tth->set_current(geometry.delta()->get_current());
          }
        else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" must be set to zero");
      }

    } // namespace twoC
  } // namespace geometry
} // namespace hkl
