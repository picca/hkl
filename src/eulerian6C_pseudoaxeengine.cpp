
#include "eulerian6C_pseudoaxeengine.h"
#include "parameter.h"
#include "axe_rotation.h"
#include "pseudoaxe.h"
#include "axe.h"

namespace hkl {

namespace eulerian6C {

namespace pseudoAxeEngine {

Tth::Tth(hkl::eulerian6C::Geometry & geometry) :
  hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>(geometry, false, true, false),
  _gamma(geometry.gamma()),
  _gamma0(0),
  _delta(geometry.delta()),
  _delta0(0)
{
  // Bouml preserved body begin 00033082
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
      // fill the relatedAxes
      _relatedAxes.push_back(_gamma);
      _relatedAxes.push_back(_delta);
      connect();
      Tth::update();
      
      
      // update the write part from the read part for the first time.
      _tth_w.set_current(_tth_r.get_current());
  // Bouml preserved body end 00033082
}

Tth::~Tth() 
{
  // Bouml preserved body begin 00033102
      delete _direction;
      
      delete _tth;
  // Bouml preserved body end 00033102
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Tth::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033182
    double gamma0 = _gamma->get_current().get_value();
    double delta0 = _delta->get_current().get_value();
    if (fabs(delta0) < constant::math::epsilon
        && fabs(delta0) < constant::math::epsilon)
    {
        _gamma0 = 0;
        _delta0 = 0;
        if (fabs(_direction->get_current().get_value()) < constant::math::epsilon)
            _axe0 = svector(0, 0, 1);
        else
            _axe0 = svector(0, -1, 0);
    }
    else
    {
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
            if (delta0 < 0)
              {
                _gamma0 = -gamma0;
                _delta0 = -delta0;
              }
            else
              {
                _gamma0 = gamma0;
                _delta0 = delta0;
              }
            _axe0 = svector(0, -sin(_delta0), sin(_gamma0)*cos(_delta0));
            _axe0 = _axe0.normalize();
    std::cout << _axe0 << std::endl;
          }
    }
    _initialized = true;
    _writable = true;
    this->update();
    this->set_write_from_read();
  // Bouml preserved body end 00033182
}

void Tth::update() 
{
  // Bouml preserved body begin 00033282
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
                  if (_direction->get_current().get_value() == 1.)
                    {
                      if (fabs(gamma) > constant::math::epsilon)
                        {
                          _writable = false;
                          _minmax(_tth_r, _gamma, _delta);
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
                          _minmax(_tth_r, _gamma, _delta);
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
                      _minmax(_tth_r, _gamma, _delta);
                    }
                  else
                    {
                      _writable = false;
                      _minmax(_tth_r, _gamma, _delta);
                    }
                }
            }
          else
            {
            if (delta < 0)
              tth = -tth;
            _tth_r.set_current(tth);
            _minmax(_tth_r, _gamma, _delta);
            }
        }
  // Bouml preserved body end 00033282
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Tth::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033302
      if (_initialized && _writable)
        {
          svector ki = _geometry.get_source().getKi();
          svector kf = ki.rotatedAroundVector(_axe0, _tth_w.get_current().get_value());
      
          // 1st solution
          double gamma1 = atan2(kf.y(), kf.x());
          double delta1 = atan2(kf.z(), sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
          hkl::eulerian6C::Geometry g1(0, 0, 0, 0, gamma1, delta1);
      
          // 2nd solution
          double gamma2 = atan2(-kf.y(), -kf.x());
          double delta2 = atan2(kf.z(), -sqrt(kf.x()*kf.x()+kf.y()*kf.y()));
          hkl::eulerian6C::Geometry g2(0, 0, 0, 0, gamma2, delta2);
      
          // keep the closest one.
          unconnect();
          if (_geometry.get_distance(g1) < _geometry.get_distance(g2))
            {
              _gamma->set_current(gamma1);
              _delta->set_current(delta1);
            }
          else
            {
              _gamma->set_current(gamma2);
              _delta->set_current(delta2);
            }
          this->connect();
          Tth::update();
        }
      else
        HKLEXCEPTION("Cannot set_current on Tth before it was initialized or writable.", "Please initialize it.");
  // Bouml preserved body end 00033302
}

void Tth::set_write_from_read() 
{
  // Bouml preserved body begin 00038682
      _tth_w.set_current(_tth_r.get_current().get_value());
  // Bouml preserved body end 00038682
}

/**
 * @brief print on a stream the content of the Tth
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Tth::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00033382
      ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->toStream(flux);
      _direction->toStream(flux);
      _axe0.toStream(flux);
      _tth_r.toStream(flux);
      _tth_w.toStream(flux);
      flux << " " << _gamma0;
      flux << " " << _delta0 << endl;
      
      return flux;
  // Bouml preserved body end 00033382
}

/**
 * @brief restore the content of the Tth from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Tth::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00033402
      ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->fromStream(flux);
      _direction->fromStream(flux);
      _axe0.fromStream(flux);
      _tth_r.fromStream(flux);
      _tth_w.fromStream(flux);
      flux >> _gamma0 >> _delta0;
      
      return flux;
  // Bouml preserved body end 00033402
}

void Tth::_minmax(hkl::Range & range, const hkl::Axe * gamma, const hkl::Axe * delta) 
{
  // Bouml preserved body begin 00033202
  // now compute the min and max of tth.
  if (delta->get_min() <= 0 && delta->get_max() >= 0)
  {
    double min = delta->get_min().get_value();
    double max = delta->get_max().get_value();
    Range r1(cos(*gamma));
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
    range = cos(*gamma);
    range *= cos(*delta);
    range = acos(range);
    if (delta->get_current().get_value() < 0)
      range *= -1;
  }
  // Bouml preserved body end 00033202
}

Q::Q(hkl::eulerian6C::Geometry & geometry) :
  hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>(geometry, false, true, false),
  _gamma(geometry.gamma()),
  _delta(geometry.delta())
{
  // Bouml preserved body begin 00033482
      _tth_engine = new hkl::eulerian6C::pseudoAxeEngine::Tth(geometry);
      _tth = _tth_engine->pseudoAxes()["tth"];
      
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
      // fill the relatedAxes
      _relatedAxes.push_back(_gamma);
      _relatedAxes.push_back(_delta);
      connect();
      Q::update();
      
      // update the write part from the read part for the first time.
      _q_w.set_current(_q_r.get_current());
  // Bouml preserved body end 00033482
}

Q::~Q() 
{
  // Bouml preserved body begin 00033502
      delete _tth_engine;
      
      delete _q;
  // Bouml preserved body end 00033502
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Q::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033582
      _tth_engine->initialize();
      _initialized = true;
      _readable = true;
      _writable = true;
      update();
      set_write_from_read();
  // Bouml preserved body end 00033582
}

void Q::update() 
{
  // Bouml preserved body begin 00033602
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
          _writable = _tth->is_writable();
        }
  // Bouml preserved body end 00033602
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Q::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033682
      if (_initialized)
        {
          unconnect();
          double lambda = _geometry.get_source().get_waveLength().get_value();
          double tth = 2 * asin(_q_w.get_current().get_value() * lambda / (2 * constant::physic::tau));
          _tth->set_current(tth);
          _writable = _tth->is_writable();
          connect();
          Q::update();
        }
      else
        HKLEXCEPTION("Can not set_current before initilization", "Initialize it.");
  // Bouml preserved body end 00033682
}

void Q::set_write_from_read() 
{
  // Bouml preserved body begin 00038702
      _q_w.set_current(_q_r.get_current().get_value());
  // Bouml preserved body end 00038702
}

/**
 * @brief print on a stream the content of the Q
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Q::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00033702
      ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->toStream(flux);
      _tth_engine->toStream(flux);
      _q_r.toStream(flux);
      _q_w.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00033702
}

/**
 * @brief restore the content of the Q from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Q::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00033782
      ((hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry> *)this)->fromStream(flux);
      _tth_engine->fromStream(flux);
      _q_r.fromStream(flux);
      _q_w.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00033782
}

void Q::_minmax(hkl::Range & range, const hkl::Range & gamma, const hkl::Range & delta) 
{
  // Bouml preserved body begin 00033802
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
  // Bouml preserved body end 00033802
}


} // namespace hkl::eulerian6C::pseudoAxeEngine

} // namespace hkl::eulerian6C

} // namespace hkl
