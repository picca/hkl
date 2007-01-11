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
        Eulerians::Eulerians(geometry::kappa4C::Vertical & geometry, vector<string> const & names) :
            PseudoAxeEngineTemp<geometry::kappa4C::Vertical>(geometry, "hkl", "engine", names, true, true, true),
            _alpha(geometry.get_alpha()),
            _komega(geometry._komega),
            _kappa(geometry._kappa),
            _kphi(geometry._kphi)
        {
          // parameters
          _solution = new Parameter("solution", "Switch between solution 0 or 1(default)\n",
                                    0, 1, 1);
          _parameters.add(_solution);

          for(unsigned int i=0; i<names.size(); i++)
          {
            _ranges.push_back(new Range(-constant::math::pi, 0, constant::math::pi));
          }

          // add all the PseudoMultiAxes
          _omega = new PseudoMultiAxe("omega", "omega", *_ranges[0], this);
          _chi = new PseudoMultiAxe("chi", "chi", *_ranges[1], this);
          _phi = new PseudoMultiAxe("phi", "phi", *_ranges[2], this);
          _pseudoAxes.push_back(_omega);
          _pseudoAxes.push_back(_chi);
          _pseudoAxes.push_back(_phi);

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
        Eulerians::initialize(void) throw (HKLException)
        {
          _initialized = true;
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
