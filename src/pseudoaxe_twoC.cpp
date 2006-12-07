#include "pseudoaxe_twoC.h"

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace twoC
      {
      namespace vertical
        {

        /*******************/
        /* TH2TH PSEUDOAXE */
        /*******************/
        Th2th::Th2th(geometry::twoC::Vertical & geometry) :
            PseudoAxeTemp<geometry::twoC::Vertical>(geometry, "th2th", "domega = 1/2 * d2theta."),
            _omega(geometry.omega()),
            _tth(geometry.tth()),
            _omega0(0.),
            _tth0(0.)
        {
          // this pseudoAxe is always readable
          _readable = true;
          _omega->add_observer(this);
          _tth->add_observer(this);
          connect();
          update();
        }

        void
        Th2th::initialize(void) throw (HKLException)
        {
          _omega0 = _omega->get_current().get_value();
          _tth0 = _tth->get_current().get_value();
          _initialized = true;
          _writable = true;
          connect();
          update();
        }

        void
        Th2th::update(void)
        {
          if (_connected)
            {
              // this pseudoAxe is always readable
              double omega_min = _omega->get_min().get_value();
              double omega_max = _omega->get_max().get_value();

              double min = _tth->get_min().get_value();
              if ((_omega0 - omega_min) < (_tth0 - min) / 2.)
                min = _tth0 + (omega_min - _omega0) * 2.;

              double max = _tth->get_max().get_value();
              if ((omega_max - _omega0) < (max - _tth0) / 2.)
                max = _tth0 + (omega_max - _omega0) * 2.;

              double current = _tth->get_current().get_value();
              _range.set(min, current, max);
            }
        }

        void
        Th2th::set_current(Value const & value) throw (HKLException)
        {
          bool valid = false;
          _writable = false;
          if (_initialized)
            {
              double omega = _omega->get_current().get_value();
              double tth = _tth->get_current().get_value();

              if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon_0)
                {
                  _writable = true;
                  valid = true;

                  tth = value.get_value();
                  omega = _omega0 + (tth - _tth0) / 2.;

                  // unconnect the update function to avoid computation for each set_current
                  unconnect();
                  _omega->set_current(omega);
                  _tth->set_current(tth);
                  // connect again the update method
                  connect();
                  // do the update.
                  update();
                }
              else
                HKLEXCEPTION("The pseudoAxe is not valid", "Please re-initialize it.");
            }
          else
            HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
        }

        ostream &
        Th2th::toStream(ostream & flux) const
          {
            PseudoAxeTemp<geometry::twoC::Vertical>::toStream(flux);
            flux << " " << _omega0
            << " " << _tth0 << endl;
            return flux;
          }

        istream &
        Th2th::fromStream(istream & flux)
        {
          PseudoAxeTemp<geometry::twoC::Vertical>::fromStream(flux);
          flux >> _omega0 >> _tth0;

          return flux;
        }

        /******************/
        /* Q2TH PSEUDOAXE */
        /******************/
        Q2th::Q2th(geometry::twoC::Vertical & geometry) :
            PseudoAxeTemp<geometry::twoC::Vertical>(geometry, "q2th", "domega = 1/2 * d2theta."),
            _omega(geometry.omega()),
            _tth(geometry.tth())
        {
          // this pseudoAxe is always readable
          _readable = true;
          _omega->add_observer(this);
          _tth->add_observer(this);
          update();
        }

        Q2th::~Q2th(void)
        {}

        void
        Q2th::initialize(void) throw (HKLException)
        {
          _omega0 = _omega->get_current().get_value();
          _tth0 = _tth->get_current().get_value();
          _initialized = true;
          _writable = true;
          connect();
          update();
        }

        void
        Q2th::update(void)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();
          double min = -2 * constant::physic::tau / lambda;
          double max = 2 * constant::physic::tau / lambda;

          // next lines will not be executed if the source is not well set.
          double theta = _tth->get_current().get_value() / 2.;
          double q = 2 * constant::physic::tau * sin(theta) / lambda;
          _range.set(min, q, max);
        }

        void
        Q2th::set_current(Value const & value) throw (HKLException)
        {
          bool valid = false;
          _writable = false;
          if (_initialized)
            {
              double omega = _omega->get_current().get_value();
              double tth = _tth->get_current().get_value();

              if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon_0)
                {
                  _writable = true;
                  valid = true;

                  double lambda = _geometry.get_source().get_waveLength().get_value();

                  tth = 2 * asin(value.get_value() * lambda / (2 * constant::physic::tau));
                  omega = _omega0 + (tth - _tth0) / 2.;

                  _omega->set_current(omega);
                  _tth->set_current(tth);
                }
              else
                HKLEXCEPTION("The pseudoAxe is not valid", "Please re-initialize it.");
            }
          else
            HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
        }

        ostream &
        Q2th::toStream(ostream & flux) const
          {
            PseudoAxeTemp<geometry::twoC::Vertical>::toStream(flux);
            flux << " " << _omega0
            << " " << _tth0 << endl;
            return flux;
          }

        istream &
        Q2th::fromStream(istream & flux)
        {
          PseudoAxeTemp<geometry::twoC::Vertical>::fromStream(flux);
          flux >> _omega0 >> _tth0;

          return flux;
        }

        /***************/
        /* Q PSEUDOAXE */
        /***************/
        Q::Q(geometry::twoC::Vertical & geometry) :
            PseudoAxeTemp<geometry::twoC::Vertical>(geometry, "q", "q = 2 * tau * sin(theta) / lambda"),
            _tth(geometry.tth())
        {
          _readable = true;
          _tth->add_observer(this);
          update();
        }

        Q::~Q(void)
        {}

        void
        Q::initialize(void) throw (HKLException)
        {
          _initialized = true;
          _writable = true;
          connect();
          update();
        }

        void
        Q::update(void)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();
          double min = -2 * constant::physic::tau / lambda;
          double max = 2 * constant::physic::tau / lambda;
          double theta = _tth->get_current().get_value() / 2.;
          double q = 2 * constant::physic::tau * sin(theta) / lambda;
          _range.set(min, q, max);
        }

        void
        Q::set_current(Value const & value) throw (HKLException)
        {
          bool valid = false;
          _writable = false;
          if (_initialized)
            {
              _writable = true;
              valid = true;

              double lambda = _geometry.get_source().get_waveLength().get_value();
              double tth = 2 * asin(value.get_value() * lambda / (2 * constant::physic::tau));
              _tth->set_current(tth);
            }
          else
            HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
        }

      } // namespace vertical
    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl
