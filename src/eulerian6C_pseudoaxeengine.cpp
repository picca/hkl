
#include "eulerian6C_pseudoaxeengine.h"
#include "parameter.h"
#include "axe_rotation.h"
#include "pseudoaxe.h"

namespace hkl
  {

  namespace eulerian6C
    {

    namespace pseudoAxeEngine
      {

      Tth::Tth(hkl::eulerian6C::Geometry & geometry) :
          hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>(geometry, false, true, false),
          _gamma(geometry.gamma()),
          _gamma0(0),
          _delta(geometry.delta()),
          _delta0(0)
      {
        // parameters
        _direction = new Parameter("direction", "Prefered mode when gamma=0 and delta=0\n  Vertical=1(default).\n  Horizontal=0.",
                                   0, 1, 1);
        _parameters.add(_direction);

        // add the pseudoAxe
        _tth = new PseudoAxe("tth", "tth", this);
        _pseudoAxes.push_back(_tth);

        // add the observers
        _gamma->add_observer(this);
        _delta->add_observer(this);

        // fill the relatedAxes
        _relatedAxes.push_back(_gamma);
        _relatedAxes.push_back(_delta);
        this->connect();
        Tth::update();
      }

      Tth::~Tth()
      {
        delete _direction;

        delete _tth;
      }

      /**
       * @brief Initialize the pseudoAxe.
       *
       * This method must be call before using a pseudoAxe.
       */
      void Tth::initialize() throw(hkl::HKLException)
      {
        double gamma0 = _gamma->get_current().get_value();
        double delta0 = _delta->get_current().get_value();
        if (fabs(delta0) < constant::math::epsilon
            && fabs(delta0) < constant::math::epsilon)
          {
            _gamma0 = 0;
            _delta0 = 0;
            if (fabs(_direction->get_current().get_value()) < constant::math::epsilon)
              {
                _axe0.data[X] = 0;
                _axe0.data[Y] = 0;
                _axe0.data[Z] = 1;
              }
            else
              {
                _axe0.data[X] = 0;
                _axe0.data[Y] = -1;
                _axe0.data[Z] = 0;
              }
          }
        else
          {
            if (fabs(delta0) < constant::math::epsilon)
              {
                _direction->set_current(0);
                _gamma0 = 0;
                _delta0 = 0;

                _axe0.data[X] = 0;
                _axe0.data[Y] = 0;
                _axe0.data[Z] = 1;
              }
            else if (fabs(gamma0) < constant::math::epsilon)
              {
                _direction->set_current(1);
                _gamma0 = 0;
                _delta0 = 0;

                _axe0.data[X] = 0;
                _axe0.data[Y] = -1;
                _axe0.data[Z] = 0;
              }
            else
              {
                if (delta0 < 0)
                  {
                    _gamma0 = -gamma0;
                    _delta0 = -delta0;
                  }
                else
                  {
                    _gamma0 = gamma0;
                    _delta0 = delta0;

                    _axe0.data[X] = 0;
                    _axe0.data[Y] = -sin(_delta0);
                    _axe0.data[Z] = sin(_gamma0)*cos(_delta0);
                  }
              }
          }
        _initialized = true;
        _writable = true;
        Tth::update();
      }

      void Tth::update()
      {
        if (_connected)
          {
            // compute the min max range.
            double min, max;
            this->compute_tth_range(min, max);

            // compute the current, consign and writability of tth
            double gamma = _gamma->get_current().get_value();
            double delta = _delta->get_current().get_value();
            bool shit;
            double current = this->compute_tth(gamma, delta, shit);

            gamma = _gamma->get_consign().get_value();
            delta = _delta->get_consign().get_value();
            double consign = this->compute_tth(gamma, delta, _writable);

            // if the pseudoAxe was not initialized it is not writable.
            if (!_initialized)
              _writable = false;

            this->set_pseudoAxe(_tth, min, current, consign, max);
          }
      }

      /**
       * @brief set the current value of the PseudoAxe.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      void Tth::set() throw(hkl::HKLException)
      {
        if (_initialized && _writable)
          {
            hkl_svector kf;

            _geometry.get_source().get_ki(&kf);
            ::hkl_svector_rotated_around_vector(&kf, &_axe0, _tth->get_consign().get_value());

            // 1st solution
            double gamma1 = atan2(kf.data[Y], kf.data[X]);
            double delta1 = atan2(kf.data[Z], sqrt(kf.data[X]*kf.data[X]+kf.data[Y]*kf.data[Y]));
            hkl::eulerian6C::Geometry g1(0, 0, 0, 0, gamma1, delta1);

            // 2nd solution
            double gamma2 = atan2(-kf.data[Y], -kf.data[X]);
            double delta2 = atan2(kf.data[Z], -sqrt(kf.data[X]*kf.data[X]+kf.data[Y]*kf.data[Y]));
            hkl::eulerian6C::Geometry g2(0, 0, 0, 0, gamma2, delta2);

            // keep the closest one.
            this->unconnect();
            if (_geometry.get_distance(g1) < _geometry.get_distance(g2))
              {
                _gamma->set_consign(gamma1);
                _delta->set_consign(delta1);
              }
            else
              {
                _gamma->set_consign(gamma2);
                _delta->set_consign(delta2);
              }
            this->connect();
            Tth::update();
          }
        else
          HKLEXCEPTION("Cannot set_current on Tth before it was initialized or writable.", "Please initialize it.");
      }

      /**
       * @brief print on a stream the content of the Tth
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & Tth::toStream(std::ostream & flux) const
        {
          ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->toStream(flux);
          _direction->toStream(flux);
          flux << " " << _gamma0;
          flux << " " << _delta0 << std::endl;

          return flux;
        }

      /**
       * @brief restore the content of the Tth from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & Tth::fromStream(std::istream & flux)
      {
        ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->fromStream(flux);
        _direction->fromStream(flux);
        flux >> _gamma0 >> _delta0;

        return flux;
      }

      /**
       * @brief Compute the tth angle from gamma and delta
       * @param gamma The gamma angle.
       * @param delta The gamma angle.
       * @param writable Is the axe compatible with the initialization.
       * @return the tth angle.
       */
      double Tth::compute_tth(double gamma, double delta, bool & writable)
      {
        double tth = ::acos(::cos(gamma)*::cos(delta));

        // ki colinear to kf0.
        if (fabs(_gamma0) < constant::math::epsilon
            && fabs(_delta0) < constant::math::epsilon)
          {
            if (_direction->get_current().get_value() == 1.)
              {
                if (::fabs(gamma) > constant::math::epsilon)
                  writable = false;
                else
                  writable = true;
              }
            else
              {
                if (::fabs(delta) > constant::math::epsilon)
                  writable = false;
                else
                  writable = true;
              }
          }
        else // ki not colinear to kf0
          {
            // check if ki^kf colinear to the tth axis of rotation _axe0
            if ( ::fabs(::sin(_delta0)*::sin(gamma)*::cos(delta) - ::sin(delta)*::sin(_gamma0)*::cos(_delta0)) < constant::math::epsilon)
              {
                // yes so check if the axe is colinear or anti-colinear.
                writable = true;
                if (_axe0.data[Y] * -::sin(delta) < 0 && _axe0.data[Z] * ::sin(gamma)*cos(delta) < 0)
                  tth *= -1;
              }
            else
              writable = false;
          }
        return tth;
      }

      /**
       * @brief compute the range of the tth pseudoAxe.
       * @param min the minimum value computed
       * @param max the maximum value computed
       */
      void Tth::compute_tth_range(double & min, double & max)
      {

        // compute the range of cos(gamma)
        hkl::Interval i_gamma(_gamma->get_min().get_value(), _gamma->get_max().get_value());
        i_gamma.cos();

        // now the tth range depending on the delta range.
        if (_delta->get_min() <= 0 && _delta->get_max() >= 0) // delta contain zero.
          {
            // compute the minimum using delta [min, 0]
            hkl::Interval i_delta(_delta->get_min().get_value(), 0);
            i_delta.cos();
            i_delta *= i_gamma;
            i_delta.acos();
            min = -i_delta.get_max();

            // compute the maximum using delta [0, max]
            i_delta.set_min(0);
            i_delta.set_max(_delta->get_max().get_value());
            i_delta.cos();
            i_delta *= i_gamma;
            i_delta.acos();
            max = i_delta.get_max();
          }
        else
          {
            hkl::Interval i_delta(_delta->get_min().get_value(), _delta->get_max().get_value());
            i_gamma.cos();
            i_delta.cos();
            i_delta *= i_gamma;
            i_delta.acos();

            min = i_delta.get_min();
            max = i_delta.get_max();
          }
      }

      Q::Q(hkl::eulerian6C::Geometry & geometry) :
          hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>(geometry, false, true, false),
          _gamma(geometry.gamma()),
          _delta(geometry.delta())
      {
        _tth_engine = new hkl::eulerian6C::pseudoAxeEngine::Tth(geometry);
        _tth = _tth_engine->pseudoAxes()["tth"];

        _parameters = _tth_engine->parameters();

        // add the pseudoAxe
        _q = new PseudoAxe("q", "q = 2 * tau * sin(theta) / lambda", this);
        _pseudoAxes.push_back(_q);

        // add the observers
        _gamma->add_observer(this);
        _delta->add_observer(this);

        // fill the relatedAxes
        _relatedAxes.push_back(_gamma);
        _relatedAxes.push_back(_delta);
        this->connect();
        Q::update();
      }

      Q::~Q()
      {
        delete _tth_engine;

        delete _q;
      }

      /**
       * @brief Initialize the pseudoAxe.
       *
       * This method must be call before using a pseudoAxe.
       */
      void Q::initialize() throw(hkl::HKLException)
      {
        _tth_engine->initialize();
        _initialized = true;
        _readable = true;
        _writable = true;
        Q::update();
      }

      void Q::update()
      {
        if (_connected)
          {
            double lambda = _geometry.get_source().get_waveLength().get_value();

            double min, max;
            this->compute_q_range(min, max);

            double theta = _tth->get_current().get_value() / 2.;
            double theta_c = _tth->get_consign().get_value() / 2.;
            double f = 2 * constant::physic::tau / lambda;
            double current = f * sin(theta);
            double consign = f * sin(theta_c);

            _writable = _tth->is_writable();
            this->set_pseudoAxe(_q, min, current, consign, max);
          }
      }

      /**
       * @brief set the current value of the PseudoAxe.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      void Q::set() throw(hkl::HKLException)
      {
        if (_initialized)
          {
            this->unconnect();
            double lambda = _geometry.get_source().get_waveLength().get_value();
            double tth = 2 * asin(_q->get_consign().get_value() * lambda / (2 * constant::physic::tau));
            _tth->set_consign(tth);
            _writable = _tth->is_writable();
            this->connect();
            Q::update();
          }
        else
          HKLEXCEPTION("Can not set_current before initilization", "Initialize it.");
      }

      /**
       * @brief print on a stream the content of the Q
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & Q::toStream(std::ostream & flux) const
        {
          ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->toStream(flux);
          _tth_engine->toStream(flux);

          return flux;
        }

      /**
       * @brief restore the content of the Q from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & Q::fromStream(std::istream & flux)
      {
        ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->fromStream(flux);
        _tth_engine->fromStream(flux);

        return flux;
      }

      /**
       * @brief compute the range of the Q pseudoAxe.
       * @param min the minimum value computed
       * @param max the maximum value computed
       */
      void Q::compute_q_range(double & min, double & max)
      {
        // now compute the min and max of tth.
        hkl::Interval i(_tth->get_min().get_value() / 2., _tth->get_max().get_value() / 2.);
        i.sin();
        double lambda = _geometry.get_source().get_waveLength().get_value();

        double f = 2 * constant::physic::tau / lambda;
        min = f * i.get_min();
        max = f * i.get_max();
      }


    } // namespace hkl::eulerian6C::pseudoAxeEngine

  } // namespace hkl::eulerian6C

} // namespace hkl
