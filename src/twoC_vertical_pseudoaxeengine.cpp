
#include "twoC_vertical_pseudoaxeengine.h"
#include "axe_rotation.h"
#include "pseudoaxe.h"

namespace hkl {

namespace twoC {

namespace vertical {

namespace pseudoAxeEngine {

Th2th::Th2th(hkl::twoC::vertical::Geometry & geometry) :
  PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>(geometry, false, true, false),
  _omega(geometry.omega()),
  _omega0(0),
  _tth(geometry.tth()),
  _tth0(0) 
{
  // Bouml preserved body begin 00031E02
  // add all the PseudoAxes
    _th2th = new PseudoAxe( "th2th", "domega = 1/2 * d2theta.", _th2th_r, _th2th_w, this);
    _pseudoAxes.push_back(_th2th);

    // add observer to observable
    _omega->add_observer(this);
    _tth->add_observer(this);

    // fill the relatedAxes;
    _relatedAxes.push_back(_omega);
    _relatedAxes.push_back(_tth);

    connect();
    Th2th::update();

    // update the write part from the read part for the first time.
    _th2th_w.set_current(_th2th_r.get_current());
  // Bouml preserved body end 00031E02
}

Th2th::~Th2th() 
{
  // Bouml preserved body begin 00031E82
      delete _th2th;
  // Bouml preserved body end 00031E82
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Th2th::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00031F02
      _omega0 = _omega->get_current().get_value();
      _tth0 = _tth->get_current().get_value();
      _initialized = true;
      _writable = true;
      Th2th::update();
      this->set_write_from_read();
  // Bouml preserved body end 00031F02
}

void Th2th::update() 
{
  // Bouml preserved body begin 00031F82
      if (_connected)
        {
          // this pseudoAxe is always readable
          // now compute the tth min max range
          double omega_min = _omega->get_min().get_value();
          double omega_max = _omega->get_max().get_value();
      
          double min = _tth->get_min().get_value();
          if ((_omega0 - omega_min) < (_tth0 - min) / 2.)
            min = _tth0 + (omega_min - _omega0) * 2.;
      
          double max = _tth->get_max().get_value();
          if ((omega_max - _omega0) < (max - _tth0) / 2.)
            max = _tth0 + (omega_max - _omega0) * 2.;
          
          // compute the new current value
          double current = _tth->get_current().get_value();
          _th2th_r.set(min, current, max);
        }
  // Bouml preserved body end 00031F82
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Th2th::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032002
      _writable = false;
      if (_initialized)
        {
          double tth = _th2th_w.get_current().get_value();
          double min = _th2th_r.get_min().get_value();
          double max = _th2th_r.get_max().get_value();
          if (tth >= min && tth <= max)
            {
              double omega = _omega->get_current().get_value();
              tth = _tth->get_current().get_value();

              if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon)
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
        }
      else
        {
          HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
        }
  // Bouml preserved body end 00032002
}

void Th2th::set_write_from_read() 
{
  // Bouml preserved body begin 00038402
      _th2th_w.set_current(_th2th_r.get_current().get_value());
  // Bouml preserved body end 00038402
}

/**
 * @brief print on a stream the content of the Th2th
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Th2th::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00032082
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::toStream(flux);
      _th2th_r.toStream(flux);
      _th2th_w.toStream(flux);
      flux << " " << _omega0;
      flux << " " << _tth0;
      flux << std::endl;
      
      return flux;
  // Bouml preserved body end 00032082
}

/**
 * @brief restore the content of the Th2th from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Th2th::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00032102
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::fromStream(flux);
      _th2th_r.fromStream(flux);
      _th2th_w.fromStream(flux);
      flux >> _omega0 >> _tth0;
      
      return flux;
  // Bouml preserved body end 00032102
}

Q2th::Q2th(hkl::twoC::vertical::Geometry & geometry) :
  PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>(geometry, false, true, false),
  _omega(geometry.omega()),
  _omega0(0),
  _tth(geometry.tth()),
  _tth0(0) 
{
  // Bouml preserved body begin 00032182

    // add all the PseudoAxes
    _q2th = new PseudoAxe( "q2th", "domega = 1/2 * d2theta.", _q2th_r, _q2th_w, this);
    _pseudoAxes.push_back(_q2th);

    // add observer to observable
    _omega->add_observer(this);
    _tth->add_observer(this);

    //fill the relatedAxes
    _relatedAxes.push_back(_omega);
    _relatedAxes.push_back(_tth);

    this->connect();
    Q2th::update();

    // update the write part from the read part for the first time.
    _q2th_w.set_current(_q2th_r.get_current());
  // Bouml preserved body end 00032182
}

Q2th::~Q2th() 
{
  // Bouml preserved body begin 00032202
      delete _q2th;
  // Bouml preserved body end 00032202
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Q2th::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032282
      _omega0 = _omega->get_current().get_value();
      _tth0 = _tth->get_current().get_value();
      _initialized = true;
      _writable = true;
      Q2th::update();
      this->set_write_from_read();
  // Bouml preserved body end 00032282
}

void Q2th::update() 
{
  // Bouml preserved body begin 00032302
      if (_connected)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();

          // now compute the tth min max range
          double omega_min = _omega->get_min().get_value();
          double omega_max = _omega->get_max().get_value();
      
          double min = _tth->get_min().get_value();
          if ((_omega0 - omega_min) < (_tth0 - min) / 2.)
            min = _tth0 + (omega_min - _omega0) * 2.;
          min = 2 * constant::physic::tau * sin(min/2.) / lambda;

          double max = _tth->get_max().get_value();
          if ((omega_max - _omega0) < (max - _tth0) / 2.)
            max = _tth0 + (omega_max - _omega0) * 2.;
          max = 2 * constant::physic::tau * sin(max/2.) / lambda;
          
          // compute the new current value
          double theta = _tth->get_current().get_value() / 2.;
          double q = 2 * constant::physic::tau * sin(theta) / lambda;
          _q2th_r.set(min, q, max);
        }
  // Bouml preserved body end 00032302
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Q2th::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032382
      _writable = false;
      if (_initialized)
        {
          double omega = _omega->get_current().get_value();
          double tth = _tth->get_current().get_value();
      
          if (fabs(omega - _omega0 - (tth - _tth0) / 2) < constant::math::epsilon)
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
  // Bouml preserved body end 00032382
}

void Q2th::set_write_from_read() 
{
  // Bouml preserved body begin 00038482
      _q2th_w.set_current(_q2th_r.get_current().get_value());
  // Bouml preserved body end 00038482
}

/**
 * @brief print on a stream the content of the Q2th
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Q2th::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00032402
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::toStream(flux);
      _q2th_r.toStream(flux);
      _q2th_w.toStream(flux);
      flux << " " << _omega0;
      flux << " " << _tth0;
      flux << std::endl;
      
      return flux;
  // Bouml preserved body end 00032402
}

/**
 * @brief restore the content of the Q2th from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Q2th::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00032482
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::fromStream(flux);
      _q2th_r.fromStream(flux);
      _q2th_w.fromStream(flux);
      flux >> _omega0 >> _tth0;
      
      return flux;
  // Bouml preserved body end 00032482
}

Q::Q(hkl::twoC::vertical::Geometry & geometry) :
  PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>(geometry, false, true, false),
  _tth(geometry.tth())
{
  // Bouml preserved body begin 00032502
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
      
      // fill the relatedAxes
      _relatedAxes.push_back(_tth);
      
      Q::connect();
      Q::update();
      
      // update the write part from the read part for the first time.
      _q_w.set_current(_q_r.get_current());
  // Bouml preserved body end 00032502
}

Q::~Q() 
{
  // Bouml preserved body begin 00032582
      delete _q;
  // Bouml preserved body end 00032582
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Q::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032602
      _initialized = true;
      _writable = true;
      set_write_from_read();
  // Bouml preserved body end 00032602
}

void Q::update() 
{
  // Bouml preserved body begin 00032682
      if (_connected)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();
          double min = -2 * constant::physic::tau / lambda;
          double max = 2 * constant::physic::tau / lambda;
          double theta = _tth->get_current().get_value() / 2.;
          double q = 2 * constant::physic::tau * sin(theta) / lambda;
          _q_r.set(min, q, max);
        }
  // Bouml preserved body end 00032682
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Q::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032702
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
  // Bouml preserved body end 00032702
}

void Q::set_write_from_read() 
{
  // Bouml preserved body begin 00038502
      _q_w.set_current(_q_r.get_current().get_value());
  // Bouml preserved body end 00038502
}

/**
 * @brief print on a stream the content of the Q
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Q::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00032782
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::toStream(flux);
      _q_r.toStream(flux);
      _q_w.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00032782
}

/**
 * @brief restore the content of the Q from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Q::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00032802
      PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>::fromStream(flux);
      _q_r.fromStream(flux);
      _q_w.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00032802
}


} // namespace hkl::twoC::vertical::pseudoAxeEngine

} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
