#include "pseudoaxeengine_eulerian6C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace eulerian6C
      {

      /*****************/
      /* TTH PSEUDOAXE */
      /*****************/
      Tth::Tth(geometry::Eulerian6C & geometry) :
          PseudoAxeEngineTemp<geometry::Eulerian6C>(geometry, false, true, false),
          _gamma(geometry.gamma()),
          _delta(geometry.delta())
      {
        // parameters
        _direction = new Parameter("direction", "Prefered mode when gamma=0 and delta=0\n  Vertical=1(default).\n  Horizontal=0.",
                                   0, 1, 1);
        _parameters.add(_direction);

        // set the range
        _tth_r.set_range(-constant::math::pi, constant::math::pi);
        _tth_w.set_range(-constant::math::pi, constant::math::pi);

        // add the pseudoAxe
        _tth = new PseudoAxe("tth", "tth", _tth_r, _tth_w, this);
        _pseudoAxes.push_back(_tth);

        // add the observers
        _gamma->add_observer(this);
        _delta->add_observer(this);
        connect();
        Tth::update();


        // update the write part from the read part for the first time.
        _tth_w.set_current(_tth_r.get_current());
      }

      Tth::~Tth(void)
      {
        delete _direction;

        delete _tth;
      }

      ostream &
      Tth::toStream(ostream & flux) const
        {
          PseudoAxeEngineTemp<geometry::Eulerian6C>::toStream(flux);
          _direction->toStream(flux);
          _axe0.toStream(flux);
          _tth_r.toStream(flux);
          _tth_w.toStream(flux);
          flux << " " << _gamma0;
          flux << " " << _delta0 << endl;

          return flux;
        }

      istream &
      Tth::fromStream(istream & flux)
      {
        PseudoAxeEngineTemp<geometry::Eulerian6C>::fromStream(flux);
        _direction->fromStream(flux);
        _axe0.fromStream(flux);
        _tth_r.fromStream(flux);
        _tth_w.fromStream(flux);
        flux >> _gamma0 >> _delta0;

        return flux;
      }

      void
      Tth::initialize(void) throw (HKLException)
      {
        double gamma0 = _gamma->get_current().get_value();
        double delta0 = _delta->get_current().get_value();
        if (fabs(delta0) < constant::math::epsilon)
          {
            _direction->set_current(0);
            _gamma0 = 0;
            _delta0 = 0;
            _axe0 = svector(0, 0, 1);
          }
        else if (fabs(gamma0) < constant::math::epsilon)
          {
            _direction->set_current(1);
            _gamma0 = 0;
            _delta0 = 0;
            _axe0 = svector(0, -1, 0);
          }
        else
          {
            _gamma0 = gamma0;
            _delta0 = delta0;
            _axe0 = svector(0, -sin(delta0), sin(gamma0)*cos(delta0));
            _axe0 = _axe0.normalize();
          }
        _initialized = true;
        _writable = true;
      }

      void
      Tth::_minmax(Range & range, Range const & gamma, Range const & delta)
      {
        // now compute the min and max of tth.
        if (delta.contain_zero())
          {
            double min = delta.get_min().get_value();
            double max = delta.get_max().get_value();
            Range r1(cos(gamma));
            Range r2(r1);
            r1 *= cos(Range(0,0,max));
            r1 = acos(r1);
            r2 *= cos(Range(min, 0, 0));
            r2 = acos(r2);
            r2 *= -1;

            double min1 = r1.get_min().get_value();
            double max1 = r1.get_max().get_value();
            double min2 = r2.get_min().get_value();
            double max2 = r2.get_max().get_value();

            if (min1 < min2)
              min = min1;
            else
              min = min2;
            if (max1 > max2)
              max = max1;
            else
              max = max2;
            range.set_range(min, max);
          }
        else
          {
            range = cos(gamma);
            range *= cos(delta);
            range = acos(range);
            if (delta.get_current().get_value() < 0)
              range *= -1;
          }
      }

      void
      Tth::update(void)
      {
        if (_connected)
          {
            // update the current value.
            double gamma = _gamma->get_current().get_value();
            double delta = _delta->get_current().get_value();
            double tth = acos(cos(gamma)*cos(delta));
            _tth_r.set_current(tth);
            if (_initialized)
              {
                if (fabs(_gamma0) < constant::math::epsilon
                    && fabs(_delta0) < constant::math::epsilon)
                  {
                    if (_direction->get_current().get_value() == 1)
                      {
                        if (fabs(gamma) > constant::math::epsilon)
                          {
                            _writable = false;
                            _minmax(_tth_r, *_gamma, *_delta);
                          }
                        else
                          {
                            _writable = true;
                            _tth_r.set_range(_delta->get_min().get_value(), _delta->get_max().get_value());
                          }
                      }
                    else
                      {
                        if (fabs(delta) > constant::math::epsilon)
                          {
                            _writable = false;
                            _minmax(_tth_r, *_gamma, *_delta);
                          }
                        else
                          {
                            _writable = true;
                            _tth_r.set_range(_gamma->get_min().get_value(), _gamma->get_max().get_value());
                          }
                      }
                  }
                else
                  {
                    if ( fabs(sin(_delta0)*sin(gamma)*cos(delta) - sin(delta)*sin(_gamma0)*cos(_delta0)) < constant::math::epsilon)
                      {
                        _writable = true;
                        // the axe can be colinear or anti colinear.
                        if (_axe0.y() * -sin(delta) < 0 &&
                            _axe0.z() * sin(gamma)*cos(delta) < 0)
                          {
                            tth *= -1;
                            _tth_r.set_current(tth);
                          }
                        _minmax(_tth_r, *_gamma, *_delta);
                      }
                    else
                      {
                        _writable = false;
                        _minmax(_tth_r, *_gamma, *_delta);
                      }
                  }
              }
            else
              _minmax(_tth_r, *_gamma, *_delta);
          }
      }


      void
      Tth::set(void) throw (HKLException)
        {
          if (_initialized && _writable)
            {
              svector ki = _geometry.get_source().getKi();
              svector kf = ki.rotatedAroundVector(_axe0, _tth_w.get_current().get_value());

              // 1st solution
              double gamma1 = atan2(kf.y(), kf.x());
              double delta1 = atan2(kf.z(), sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
              geometry::Eulerian6C g1(0, 0, 0, 0, gamma1, delta1);

              // 2nd solution
              double gamma2 = atan2(-kf.y(), -kf.x());
              double delta2 = atan2(kf.z(), -sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
              geometry::Eulerian6C g2(0, 0, 0, 0, gamma2, delta2);

              // keep the closest one.
              unconnect();
              if (_geometry.getDistance(g1) < _geometry.getDistance(g2))
                {
                  _gamma->set_current(gamma1);
                  _delta->set_current(delta1);
                }
              else
                {
                  _gamma->set_current(gamma2);
                  _delta->set_current(delta2);
                }
              connect();
              Tth::update();
            }
          else
            HKLEXCEPTION("Cannot set_current on Tth before it was initialized or writable.", "Please initialize it.");
        }

      /***************/
      /* Q PSEUDOAXE */
      /***************/
      Q::Q(geometry::Eulerian6C & geometry) :
          PseudoAxeEngineTemp<geometry::Eulerian6C>(geometry, false, true, false),
          _gamma(geometry.gamma()),
          _delta(geometry.delta())
      {
        _tth_engine = new pseudoAxeEngine::eulerian6C::Tth(geometry);
        _tth = _tth_engine->pseudoAxes()[0];

        _parameters = _tth_engine->parameters();

        // set the range
        _q_r.set_range(-constant::math::pi, constant::math::pi);
        _q_w.set_range(-constant::math::pi, constant::math::pi);

        // add the pseudoAxe
        _q = new PseudoAxe("q", "q = 2 * tau * sin(theta) / lambda", _q_r, _q_w, this);
        _pseudoAxes.push_back(_q);

        // add the observers
        _gamma->add_observer(this);
        _delta->add_observer(this);
        connect();
        Q::update();

        // update the write part from the read part for the first time.
        _q_w.set_current(_q_r.get_current());
      }

      Q::~Q(void)
      {
        delete _tth_engine;

        delete _q;
      }

      void
      Q::initialize(void) throw (HKLException)
      {
        _tth_engine->initialize();
        _initialized = true;
        _readable = true;
        _writable = true;
      }

      void
      Q::update(void)
      {
        if (_connected)
          {
            double lambda = _geometry.get_source().get_waveLength().get_value();

            double tth_min = _tth->get_min().get_value();
            double theta = _tth->get_current().get_value() / 2.;
            double tth_max = _tth->get_max().get_value();

            double f = 2 * constant::physic::tau / lambda;
            double q_min = f * sin(tth_min);
            double q = f * sin(theta);
            double q_max = f * sin(tth_max);

            _q_r.set(q_min, q, q_max);
            _writable = _tth->get_writable();
          }
      }

      void
      Q::set(void) throw (HKLException)
        {
          if (_initialized)
            {
              unconnect();
              double lambda = _geometry.get_source().get_waveLength().get_value();
              double tth = 2 * asin(_q_w.get_current().get_value() * lambda / (2 * constant::physic::tau));
              _tth->set_current(tth);
              _writable = _tth->get_writable();
              connect();
              Q::update();
            }
          else
            HKLEXCEPTION("Can not set_current before initilization", "Initialize it.");
        }

    } // namespace eulerian6C
  } // namespace pseudoAxe
} // namespace hkl
