#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /*******************/
        /* OMEGA PSEUDOAXE */
        /*******************/
        Omega::Omega(geometry::kappa4C::Vertical & geometry) :
            PseudoAxeTemp<geometry::kappa4C::Vertical>(geometry, "omega", "This is the value of an equivalent eulerian geometry."),
            _alpha(geometry.get_alpha()),
            _komega(geometry._komega),
            _kappa(geometry._kappa),
            _kphi(geometry._kphi)
        {
          // this pseudoAxe is always valid readable and writable
          _initialized = true;
          _writable = true;
          _readable = true;

          // add observer to observable
          _komega->add_observer(this);
          _kappa->add_observer(this);
          _kphi->add_observer(this);

          connect();
          update();
        }

        Omega::~Omega(void)
        {}

        void
        Omega::update(void)
        {
          if (_connected)
            {
              double min = -constant::math::pi;
              double max = constant::math::pi;

              double const & komega = _komega->get_current().get_value();
              double const & kappa = _kappa->get_current().get_value();

              double current = komega + atan(tan(kappa/2.) * cos(_alpha)) + constant::math::pi/2.;
              _range.set(min, current, max);
            }
        }

        void
        Omega::set_current(Value const & value) throw (HKLException)
        {
          if (_initialized)
            {
              double const & omega = value.get_value();
              if (omega != _range.get_current().get_value())
                {
                  double komega;
                  double kappa = _kappa->get_current().get_value();
                  double kphi = _kphi->get_current().get_value();

                  double chi = -2 * asin(sin(kappa/2.) * sin(_alpha));
                  double phi = kphi + atan(tan(kappa/2.) * cos(_alpha)) - constant::math::pi/2.;

                  double p = asin(tan(chi/2.)/tan(_alpha));
                  komega = omega + p - constant::math::pi/2.;
                  kappa = -2 * asin(sin(chi/2.)/sin(_alpha));
                  kphi = phi + p + constant::math::pi/2.;

                  _komega->set_current(komega);
                  _kappa->set_current(kappa);
                  _kphi->set_current(kphi);
                }
            }
          else
            {
              HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
            }
        }

        /*****************/
        /* CHI PSEUDOAXE */
        /*****************/
        Chi::Chi(geometry::kappa4C::Vertical & geometry) :
            PseudoAxeTemp<geometry::kappa4C::Vertical>(geometry, "chi", "This is the value of an equivalent eulerian geometry."),
            _alpha(geometry.get_alpha()),
            _komega(geometry._komega),
            _kappa(geometry._kappa),
            _kphi(geometry._kphi)
        {
          // this pseudoAxe is always valid readable and writable
          _initialized = true;
          _writable = true;
          _readable = true;

          // add PseudoAxe chi observer to observable Axes
          _komega->add_observer(this);
          _kappa->add_observer(this);
          _kphi->add_observer(this);

          connect();
          update();
        }

        Chi::~Chi(void)
        {}

        void
        Chi::update(void)
        {
          if (_connected)
            {
              double min = -_alpha * 2.;
              double max = -min;

              double const & kappa = _kappa->get_current().get_value();

              double current = -2 * asin(sin(kappa/2.) * sin(_alpha));
              _range.set(min, current, max);
            }
        }

        void
        Chi::set_current(Value const & value) throw (HKLException)
        {
          if (_initialized)
            {
              if (value.get_value() != _range.get_current().get_value())
                {
                  if (fabs(value.get_value()) <= 2 * _alpha)
                    {

                      double komega = _komega->get_current().get_value();
                      double kappa = _kappa->get_current().get_value();
                      double kphi = _kphi->get_current().get_value();

                      double omega = komega + atan(tan(kappa/2.) * cos(_alpha)) + constant::math::pi/2.;
                      double const & chi = value.get_value();
                      double phi = kphi + atan(tan(kappa/2.) * cos(_alpha)) - constant::math::pi/2.;

                      double p = asin(tan(chi/2.)/tan(_alpha));
                      komega = omega + p - constant::math::pi/2.;
                      kappa = -2 * asin(sin(chi/2.)/sin(_alpha));
                      kphi = phi + p + constant::math::pi/2.;

                      _komega->set_current(komega);
                      _kappa->set_current(kappa);
                      _kphi->set_current(kphi);
                    }
                  else
                    {
                      ostringstream reason;
                      reason << "Unreachable \"" << get_name() << "\" PseudoAxeTemp value";
                      HKLEXCEPTION(reason.str(),
                                   "|chi| <= 2 * alpha");
                    }
                }
            }
          else
            {
              HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
            }
        }

        /*****************/
        /* PHI PSEUDOAXE */
        /*****************/
        Phi::Phi(geometry::kappa4C::Vertical & geometry) :
            PseudoAxeTemp<geometry::kappa4C::Vertical>(geometry, "phi", "This is the value of an equivalent eulerian geometry."),
            _alpha(geometry.get_alpha()),
            _komega(geometry._komega),
            _kappa(geometry._kappa),
            _kphi(geometry._kphi)
        {
          // this pseudoAxe is always valid readable and writable
          _initialized = true;
          _writable = true;
          _readable = true;

          // add observer to observable
          _komega->add_observer(this);
          _kappa->add_observer(this);
          _kphi->add_observer(this);

          connect();
          update();
        }

        Phi::~Phi(void)
        {}

        void
        Phi::update(void)
        {
          if (_connected)
            {
              double min = -constant::math::pi;
              double max = constant::math::pi;

              double const & kappa = _geometry._kappa->get_current().get_value();
              double const & kphi = _geometry._kphi->get_current().get_value();

              double current = kphi + atan(tan(kappa/2.) * cos(_alpha)) - constant::math::pi/2.;
              _range.set(min, current, max);
            }
        }

        void
        Phi::set_current(Value const & value) throw (HKLException)
        {
          if (_initialized)
            {
              if (value.get_value() != _range.get_current().get_value())
                {
                  double komega = _geometry._komega->get_current().get_value();
                  double kappa = _geometry._kappa->get_current().get_value();
                  double kphi;

                  double omega = komega + atan(tan(kappa/2.) * cos(_alpha)) + constant::math::pi/2.;
                  double chi = -2 * asin(sin(kappa/2.) * sin(_alpha));
                  double const & phi = value.get_value();

                  double p = asin(tan(chi/2.)/tan(_alpha));
                  komega = omega + p - constant::math::pi/2.;
                  kappa = -2 * asin(sin(chi/2.)/sin(_alpha));
                  kphi = phi + p + constant::math::pi/2.;

                  _geometry._komega->set_current(komega);
                  _geometry._kappa->set_current(kappa);
                  _geometry._kphi->set_current(kphi);
                }
            }
          else
            {
              HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
            }
        }

      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoAxe
} // namespace hkl
