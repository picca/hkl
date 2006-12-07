#include "pseudoaxe_eulerian6C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace eulerian6C
      {

      /*****************/
      /* TTH PSEUDOAXE */
      /*****************/
      Tth::Tth(geometry::Eulerian6C & geometry) :
          PseudoAxeTemp<geometry::Eulerian6C>(geometry, "2theta", "2theta = 2 * theta."),
          _gamma(geometry.gamma()),
          _delta(geometry.delta())
      {
        // parameters
        _direction = new Parameter("direction", "Prefered mode when gamma=0 and delta=0\n  Vertical=1(default).\n  Horizontal=0.",
                                   0, 1, 1);
        _parameters.add(_direction);

        //this pseudoAxe is always readable;
        _readable = true;

        // add the observers
        _gamma->add_observer(this);
        _delta->add_observer(this);
        // connect and update the _range.
        connect();
        update();
      }

      Tth::Tth(Tth const & pseudoAxe) :
          PseudoAxeTemp<geometry::Eulerian6C>(pseudoAxe),
          _gamma(_geometry.gamma()),
          _delta(_geometry.delta())
      {}

      Tth::~Tth(void)
      {
        delete _direction;
      }

      ostream &
      Tth::toStream(ostream & flux) const
        {
          PseudoAxeTemp<geometry::Eulerian6C>::toStream(flux);
          _direction->toStream(flux);
          _axe0.toStream(flux);
          flux << " " << _gamma0;
          flux << " " << _delta0 << endl;

          return flux;
        }

      istream &
      Tth::fromStream(istream & flux)
      {
        PseudoAxeTemp<geometry::Eulerian6C>::fromStream(flux);
        _direction->fromStream(flux);
        _axe0.fromStream(flux);
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
        _writable = true;
        _initialized = true;
        connect();
        update();
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
            _range.set_current(tth);
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
                            _minmax(_range, *_gamma, *_delta);
                          }
                        else
                          {
                            _writable = true;
                            _range.set_range(_delta->get_min().get_value(), _delta->get_max().get_value());
                          }
                      }
                    else
                      {
                        if (fabs(delta) > constant::math::epsilon)
                          {
                            _writable = false;
                            _minmax(_range, *_gamma, *_delta);
                          }
                        else
                          {
                            _writable = true;
                            _range.set_range(_gamma->get_min().get_value(), _gamma->get_max().get_value());
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
                            _range.set_current(tth);
                          }
                        _minmax(_range, *_gamma, *_delta);
                      }
                    else
                      {
                        _writable = false;
                        _minmax(_range, *_gamma, *_delta);
                      }
                  }
              }
            else
              _minmax(_range, *_gamma, *_delta);
          }
      }


      void
      Tth::set_current(Value const & value) throw (HKLException)
      {
        // check for the validity
        //cout << "writable : " << _writable << endl;
        if (_initialized && _writable)
          {
            if (_range.get_current().get_value() != value.get_value())
              {
                svector ki = _geometry.get_source().getKi();
                svector kf = ki.rotatedAroundVector(_axe0, value.get_value());

                // 1st solution
                double gamma1 = atan2(kf.y(), kf.x());
                double delta1 = atan2(kf.z(), sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
                geometry::Eulerian6C g1(0, 0, 0, 0, gamma1, delta1);

                // 2nd solution
                double gamma2 = atan2(-kf.y(), -kf.x());
                double delta2 = atan2(kf.z(), -sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
                geometry::Eulerian6C g2(0, 0, 0, 0, gamma2, delta2);

                // keep the closest one.
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
              }
          }
        else
          HKLEXCEPTION("Cannot set_current on Tth before it was initialized or writable.", "Please initialize it.");
      }

      /***************/
      /* Q PSEUDOAXE */
      /***************/
      Q::Q(geometry::Eulerian6C & geometry) :
          PseudoAxeTemp<geometry::Eulerian6C>(geometry, "q", "q = 2 * tau * sin(theta) / lambda"),
          _gamma(geometry.gamma()),
          _delta(geometry.delta())
      {
        m_tth = new pseudoAxe::eulerian6C::Tth(geometry);
        _parameters = m_tth->parameters();
        _readable = true;
        _gamma->add_observer(this);
        _delta->add_observer(this);
        connect();
        update();
      }

      Q::~Q(void)
      {
        delete m_tth;
      }

      void
      Q::initialize(void) throw (HKLException)
      {
        m_tth->initialize();
        _initialized = true;
        _readable = true;
        _writable = true;
        connect();
        update();
      }

      void
      Q::update(void)
      {
        if (_connected)
          {
            double lambda = _geometry.get_source().get_waveLength().get_value();

            double tth_min = m_tth->get_min().get_value();
            double theta = m_tth->get_current().get_value() / 2.;
            double tth_max = m_tth->get_max().get_value();

            double f = 2 * constant::physic::tau / lambda;
            double q_min = f * sin(tth_min);
            double q = f * sin(theta);
            double q_max = f * sin(tth_max);

            _range.set(q_min, q, q_max);
            _writable = m_tth->get_writable();
          }
      }

      void
      Q::set_current(Value const & value) throw (HKLException)
      {
        if (_initialized)
          {
            if (_range.get_current().get_value() != value.get_value())
              {
                double lambda = _geometry.get_source().get_waveLength().get_value();
                double tth = 2 * asin(value.get_value() * lambda / (2 * constant::physic::tau));
                m_tth->set_current(tth);
                _writable = m_tth->get_writable();
              }
          }
        else
          HKLEXCEPTION("Can not set_current before initilization", "Initialize it.");
      }

    } // namespace eulerian6C
  } // namespace pseudoAxe
} // namespace hkl
