#include "pseudomultiaxeengine_kappa4C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoMultiAxeEngine
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /*******************/
        /* OMEGA PSEUDOAXE */
        /*******************/
        Eulerians::Eulerians(geometry::kappa4C::Vertical & geometry) :
            PseudoMultiAxeEngineTemp<geometry::kappa4C::Vertical>(geometry),
            _alpha(geometry.get_alpha()),
            _komega(geometry._komega),
            _kappa(geometry._kappa),
            _kphi(geometry._kphi)
        {
          // parameters
          _solution = new Parameter("solution", "Swithch between solution 0 or 1(default)\n",
                                    0, 1, 1);
          _parameters.add(_solution);

          // add all the PseudoMultiAxes
          _omega = new PseudoMultiAxe("omega", "omega", this);
          _chi = new PseudoMultiAxe("chi", "chi", this);
          _phi = new PseudoMultiAxe("phi", "phi", this);
          _pseudoAxes.add(_omega);
          _pseudoAxes.add(_chi);
          _pseudoAxes.add(_phi);

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

        Eulerians::~Eulerians(void)
        {
          delete _solution;

          //delete _omega;
          //delete _chi;
          //delete _phi;
        }

        void
        Eulerians::update(void) throw (HKLException)
        {
          if (_connected)
            {
              double min = -constant::math::pi;
              double max = constant::math::pi;

              double const & komega = _komega->get_current().get_value();
              double const & kappa = _kappa->get_current().get_value();
              double const & kphi = _kphi->get_current().get_value();

              double omega, chi, phi;
              if (_solution->get_current().get_value())
                {
                  omega = komega - atan(tan(kappa/2.) * cos(_alpha)) - constant::math::pi/2.;
                  chi = 2 * asin(sin(kappa/2.) * sin(_alpha));
                  phi = kphi - atan(tan(kappa/2.) * cos(_alpha)) + constant::math::pi/2.;
                }
              else
                {
                  omega = komega + atan(tan(kappa/2.) * cos(_alpha)) + constant::math::pi/2.;
                  chi = -2 * asin(sin(kappa/2.) * sin(_alpha));
                  phi = kphi + atan(tan(kappa/2.) * cos(_alpha)) - constant::math::pi/2.;
                }
              
              cout << omega << " " << chi << " " << phi << endl;
              _omega->set_current(omega);
              _chi->set_current(chi);
              _phi->set_current(phi);
            }
        }

        void
        Eulerians::set(void) throw (HKLException)
          {
            if (_initialized)
              {
                double omega = _omega->get_current().get_value();
                double chi = _chi->get_current().get_value();
                double phi = _phi->get_current().get_value();
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

              cout << omega << " " << chi << " " << phi << endl;
              cout << komega << " " << kappa << " " << kphi << endl;
                unconnect();
                _komega->set_current(komega);
                _kappa->set_current(kappa);
                _kphi->set_current(kphi);
                connect();
              }
            else
              {
                HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
              }
          }

      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoMultiAxeEngine
} // namespace hkl
