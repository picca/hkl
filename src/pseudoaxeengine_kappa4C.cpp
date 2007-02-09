#include "pseudoaxeengine_kappa4C.h"

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /*******************/
        /* OMEGA PSEUDOAXE */
        /*******************/
        Eulerians::Eulerians(geometry::kappa4C::Vertical & geometry) :
            PseudoAxeEngineTemp<geometry::kappa4C::Vertical>(geometry, true, true, true),
            _alpha(geometry.get_alpha()),
            _komega(geometry.komega()),
            _kappa(geometry.kappa()),
            _kphi(geometry.kphi())
        {
          // parameters
          _solution = new Parameter("solution", "Switch between solution 0 or 1(default)\n",
                                    0, 0, 1);
          _parameters.add(_solution);

          // set the ranges
          _omega_r.set_range(-constant::math::pi, constant::math::pi);
          _omega_w.set_range(-constant::math::pi, constant::math::pi);
          _chi_r.set_range(-_alpha * 2., _alpha * 2.);
          _chi_w.set_range(-_alpha * 2., _alpha * 2.);
          _phi_r.set_range(-constant::math::pi, constant::math::pi);
          _phi_w.set_range(-constant::math::pi, constant::math::pi);

          // fill the ranges vector with the right ranges.
          /*
          _reads.push_back(&_omega_r);
          _reads.push_back(&_chi_r);
          _reads.push_back(&_phi_r);
          _writes.push_back(&_omega_w);
          _writes.push_back(&_chi_w);
          _writes.push_back(&_phi_w);
          */

          // add all the PseudoAxes
          _omega = new PseudoAxe("omega", "omega", _omega_r, _omega_w, this);
          _chi = new PseudoAxe("chi", "chi", _chi_r, _chi_w, this);
          _phi = new PseudoAxe("phi", "phi", _phi_r, _phi_w, this);
          _pseudoAxes.push_back(_omega);
          _pseudoAxes.push_back(_chi);
          _pseudoAxes.push_back(_phi);

          // add observer to observable
          _komega->add_observer(this);
          _kappa->add_observer(this);
          _kphi->add_observer(this);

          connect();
          Eulerians::update();

          // update the write part from the read part for the first time.
          _omega_w.set_current(_omega_r.get_current());
          _chi_w.set_current(_chi_r.get_current());
          _phi_w.set_current(_phi_r.get_current());
        }

        Eulerians::~Eulerians(void)
        {
          delete _solution;

          delete _omega;
          delete _chi;
          delete _phi;
        }

        void
        Eulerians::initialize(void) throw (HKLException)
        {
          _initialized = true;
          _writable = true;
        }

        void
        Eulerians::update(void) throw (HKLException)
        {
          if (_connected)
            {
              double const & komega = _komega->get_current().get_value();
              double const & kappa = _kappa->get_current().get_value();
              double const & kphi = _kphi->get_current().get_value();
              double p = atan(tan(kappa/2.) * cos(_alpha));

              double omega, chi, phi;
              if (_solution->get_current().get_value())
                {
                  omega = komega + p - constant::math::pi/2.;
                  chi = 2 * asin(sin(kappa/2.) * sin(_alpha));
                  phi = kphi + p + constant::math::pi/2.;
                }
              else
                {
                  omega = komega + p + constant::math::pi/2.;
                  chi = -2 * asin(sin(kappa/2.) * sin(_alpha));
                  phi = kphi + p - constant::math::pi/2.;
                }

              _omega_r.set_current(omega);
              _chi_r.set_current(chi);
              _phi_r.set_current(phi);
            }
        }

        void
        Eulerians::set(void) throw (HKLException)
          {
            if (_initialized)
              {
                double const & chi = _chi_w.get_current().get_value();
                if (chi < _alpha * 2)
                  {
                    double const & omega = _omega_w.get_current().get_value();
                    double const & phi = _phi_w.get_current().get_value();
                    double p = asin(tan(chi/2.)/tan(_alpha));

                    double komega, kappa, kphi;
                    if (_solution->get_current().get_value())
                      {
                        komega = omega - p + constant::math::pi/2.;
                        kappa = 2 * asin(sin(chi/2.)/sin(_alpha));
                        kphi = phi - p - constant::math::pi/2.;
                      }
                    else
                      {
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(_alpha));
                        kphi = phi + p + constant::math::pi/2.;
                      }

                    Eulerians::unconnect();
                    _komega->set_current(komega);
                    _kappa->set_current(kappa);
                    _kphi->set_current(kphi);
                    Eulerians::connect();
                    Eulerians::update();
                  }
                else
                  HKLEXCEPTION("Can not set such a Chi value", "must be < 2* alpha.");
              }
            else
              {
                HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
              }
          }

      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoAxeEngine
} // namespace hkl
