#include "pseudoaxeengine_twoC.h"

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace twoC
      {
      namespace vertical
        {

        /*******************/
        /* TH2TH PSEUDOAXE */
        /*******************/
        Th2th::Th2th(geometry::twoC::Vertical & geometry) :
            PseudoAxeEngineTemp<geometry::twoC::Vertical>(geometry, false, true, false),
            _omega(geometry._omega),
            _tth(geometry._tth),
            _omega0(0),
            _tth0(0)
        {
          // set the ranges
          double min = _tth->get_min().get_value();
          double max = _tth->get_max().get_value();
          _th2th_r.set_range(min, max);
          _th2th_w.set_range(min, max);

          // add all the PseudoAxes
          _th2th = new PseudoAxe( "th2th", "domega = 1/2 * d2theta.", _th2th_r, _th2th_w, this);
          _pseudoAxes.push_back(_th2th);

          // add observer to observable
          _omega->add_observer(this);
          _tth->add_observer(this);

          connect();
          Th2th::update();

          // update the write part from the read part for the first time.
          _th2th_w.set_current(_th2th_r.get_current());
        }

        Th2th::~Th2th(void)
        {
          delete _th2th;
        }

        void
        Th2th::initialize(void) throw (HKLException)
        {
          _omega0 = _omega->get_current().get_value();
          _tth0 = _tth->get_current().get_value();
          _initialized = true;
          _writable = true;
        }

        void
        Th2th::update(void) throw (HKLException)
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
              _th2th_r.set(min, current, max);
            }
        }

        void
        Th2th::set(void) throw (HKLException)
          {
            _writable = false;
            if (_initialized)
              {
                double omega = _omega->get_current().get_value();
                double tth = _tth->get_current().get_value();

                if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon_0)
                  {
                    _writable = true;

                    tth = _th2th_w.get_current().get_value();
                    omega = _omega0 + (tth - _tth0) / 2.;

                    // unconnect the update function to avoid computation for each set_current
                    Th2th::unconnect();
                    _omega->set_current(omega);
                    _tth->set_current(tth);
                    Th2th::connect();
                    Th2th::update();
                  }
                else
                  HKLEXCEPTION("The pseudoAxe is not valid", "Please re-initialize it.");
              }
            else
              {
                HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
              }
          }

        ostream &
        Th2th::toStream(ostream & flux) const
          {
            PseudoAxeEngineTemp<geometry::twoC::Vertical>::toStream(flux);
            _th2th_r.toStream(flux);
            _th2th_w.toStream(flux);
            flux << " " << _omega0;
            flux << " " << _tth0;
            flux << endl;

            return flux;
          }

        istream &
        Th2th::fromStream(istream & flux)
        {
          PseudoAxeEngineTemp<geometry::twoC::Vertical>::fromStream(flux);
          _th2th_r.fromStream(flux);
          _th2th_w.fromStream(flux);
          flux >> _omega0 >> _tth0;

          return flux;
        }

        /******************/
        /* Q2TH PSEUDOAXE */
        /******************/
        Q2th::Q2th(geometry::twoC::Vertical & geometry) :
            PseudoAxeEngineTemp<geometry::twoC::Vertical>(geometry, false, true, false),
            _omega(geometry._omega),
            _tth(geometry._tth),
            _omega0(0),
            _tth0(0)
        {
          // set the ranges
          double min = _tth->get_min().get_value();
          double max = _tth->get_max().get_value();
          _q2th_r.set_range(min, max);
          _q2th_w.set_range(min, max);

          // add all the PseudoAxes
          _q2th = new PseudoAxe( "q2th", "domega = 1/2 * d2theta.", _q2th_r, _q2th_w, this);
          _pseudoAxes.push_back(_q2th);

          // add observer to observable
          _omega->add_observer(this);
          _tth->add_observer(this);

          connect();
          Q2th::update();

          // update the write part from the read part for the first time.
          _q2th_w.set_current(_q2th_r.get_current());
        }

        Q2th::~Q2th(void)
        {
          delete _q2th;
        }

        void
        Q2th::initialize(void) throw (HKLException)
        {
          _omega0 = _omega->get_current().get_value();
          _tth0 = _tth->get_current().get_value();
          _initialized = true;
          _writable = true;
        }

        void
        Q2th::update(void) throw (HKLException)
        {
          if (_connected)
            {
              double lambda = _geometry.get_source().get_waveLength().get_value();
              double min = -2 * constant::physic::tau / lambda;
              double max = 2 * constant::physic::tau / lambda;

              // next lines will not be executed if the source is not well set.
              double theta = _tth->get_current().get_value() / 2.;
              double q = 2 * constant::physic::tau * sin(theta) / lambda;
              _q2th_r.set(min, q, max);
            }
        }

        void
        Q2th::set(void) throw (HKLException)
          {
            _writable = false;
            if (_initialized)
              {
                double omega = _omega->get_current().get_value();
                double tth = _tth->get_current().get_value();

                if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon_0)
                  {
                    _writable = true;

                    double lambda = _geometry.get_source().get_waveLength().get_value();

                    tth = 2 * asin(_q2th_w.get_current().get_value() * lambda / (2 * constant::physic::tau));
                    omega = _omega0 + (tth - _tth0) / 2.;

                    Q2th::unconnect();
                    _omega->set_current(omega);
                    _tth->set_current(tth);
                    Q2th::connect();
                    Q2th::update();
                  }
                else
                  HKLEXCEPTION("The pseudoAxe is not valid", "Please re-initialize it.");
              }
            else
              {
                HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
              }
          }

        ostream &
        Q2th::toStream(ostream & flux) const
          {
            PseudoAxeEngineTemp<geometry::twoC::Vertical>::toStream(flux);
            _q2th_r.toStream(flux);
            _q2th_w.toStream(flux);
            flux << " " << _omega0;
            flux << " " << _tth0;
            flux << endl;

            return flux;
          }

        istream &
        Q2th::fromStream(istream & flux)
        {
          PseudoAxeEngineTemp<geometry::twoC::Vertical>::fromStream(flux);
          _q2th_r.fromStream(flux);
          _q2th_w.fromStream(flux);
          flux >> _omega0 >> _tth0;

          return flux;
        }

        /***************/
        /* Q PSEUDOAXE */
        /***************/
        Q::Q(geometry::twoC::Vertical & geometry) :
            PseudoAxeEngineTemp<geometry::twoC::Vertical>(geometry, false, true, false),
            _tth(geometry._tth)
        {
          // set the ranges
          double min = _tth->get_min().get_value();
          double max = _tth->get_max().get_value();
          _q_r.set_range(min, max);
          _q_w.set_range(min, max);

          // add all the PseudoAxes
          _q = new PseudoAxe( "q", "domega = 1/2 * d2theta.", _q_r, _q_w, this);
          _pseudoAxes.push_back(_q);

          // add observer to observable
          _tth->add_observer(this);

          Q::connect();
          Q::update();

          // update the write part from the read part for the first time.
          _q_w.set_current(_q_r.get_current());
        }

        Q::~Q(void)
        {
          delete _q;
        }

        void
        Q::initialize(void) throw (HKLException)
        {
          _initialized = true;
          _writable = true;
        }

        void
        Q::update(void) throw (HKLException)
        {
          if (_connected)
            {
              double lambda = _geometry.get_source().get_waveLength().get_value();
              double min = -2 * constant::physic::tau / lambda;
              double max = 2 * constant::physic::tau / lambda;
              double theta = _tth->get_current().get_value() / 2.;
              double q = 2 * constant::physic::tau * sin(theta) / lambda;
              _q_r.set(min, q, max);
            }
        }

        void
        Q::set(void) throw (HKLException)
          {
            if (_initialized)
              {
                double lambda = _geometry.get_source().get_waveLength().get_value();
                double tth = 2 * asin(_q_w.get_current().get_value() * lambda / (2 * constant::physic::tau));
                Q::unconnect();
                _tth->set_current(tth);
                Q::connect();
                Q::update();
              }
            else
              {
                HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
              }
          }

        ostream &
        Q::toStream(ostream & flux) const
          {
            PseudoAxeEngineTemp<geometry::twoC::Vertical>::toStream(flux);
            _q_r.toStream(flux);
            _q_w.toStream(flux);

            return flux;
          }

        istream &
        Q::fromStream(istream & flux)
        {
          PseudoAxeEngineTemp<geometry::twoC::Vertical>::fromStream(flux);
          _q_r.fromStream(flux);
          _q_w.fromStream(flux);

          return flux;
        }

      } // namespace vertical
    } // namespace twoC
  } // namespace pseudoAxeEngine
} // namespace hkl
