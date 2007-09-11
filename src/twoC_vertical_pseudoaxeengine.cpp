
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
    _th2th = new hkl::PseudoAxe("th2th", "domega = 1/2 * d2theta.", this);
    _pseudoAxes.push_back(_th2th);

    // add observer to observable
    _omega->add_observer(this);
    _tth->add_observer(this);

    // fill the relatedAxes;
    _relatedAxes.push_back(_omega);
    _relatedAxes.push_back(_tth);

    Th2th::connect();
    Th2th::update();
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
      Th2th::update();
  // Bouml preserved body end 00031F02
}

void Th2th::update() 
{
  // Bouml preserved body begin 00031F82
    if (_connected)
      {
        // this pseudoAxe is always readable
        // Compute the min and max part of the PseudoAxe.
        double omega_min = _omega->get_min().get_value();
        double omega_max = _omega->get_max().get_value();

        double min = _tth->get_min().get_value();
        if ((_omega0 - omega_min) < (_tth0 - min) / 2.)
            min = _tth0 + (omega_min - _omega0) * 2.;

        double max = _tth->get_max().get_value();
        if ((omega_max - _omega0) < (max - _tth0) / 2.)
            max = _tth0 + (omega_max - _omega0) * 2.;

        // compute the new current and consign value
        double current = _tth->get_current().get_value();
        double consign = _tth->get_consign().get_value();

        this->set_pseudoAxe(_th2th, min, current, consign, max);

        //now compute the writabilility
        double omega_c = _omega->get_consign().get_value();
        double tth_c = _tth->get_consign().get_value();
        if (_initialized && fabs(omega_c - _omega0 - (tth_c - _tth0) / 2) < constant::math::epsilon)
            _writable = true;
        else
            _writable = false;
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
  // get the write part of the pseudoAxa and set the real axes.
    double const & tth = _th2th->get_consign().get_value();
    double omega = _omega0 + (tth - _tth0) / 2.;

    // unconnect the update function to avoid computation for each set_current
    Th2th::unconnect();
    _omega->set_consign(omega);
    _tth->set_consign(tth);
    Th2th::connect();
    Th2th::update();
  // Bouml preserved body end 00032002
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
      flux << " " << _omega0;
      flux << " " << _tth0;
      
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
    _q2th = new PseudoAxe( "q2th", "domega = 1/2 * d2theta.", this);
    _pseudoAxes.push_back(_q2th);

    // add observer to observable
    _omega->add_observer(this);
    _tth->add_observer(this);

    //fill the relatedAxes
    _relatedAxes.push_back(_omega);
    _relatedAxes.push_back(_tth);

    this->connect();
    Q2th::update();
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
      Q2th::update();
  // Bouml preserved body end 00032282
}

void Q2th::update() 
{
  // Bouml preserved body begin 00032302
      if (_connected)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();

          // now compute the [min, max] range of the PseudoAxe.
          double omega_min = _omega->get_min().get_value();
          double omega_max = _omega->get_max().get_value();

          double min = _tth->get_min().get_value();
          if ((_omega0 - omega_min) < (_tth0 - min) / 2.)
              min = _tth0 + (omega_min - _omega0) * 2.;

          double max = _tth->get_max().get_value();
          if ((omega_max - _omega0) < (max - _tth0) / 2.)
              max = _tth0 + (omega_max - _omega0) * 2.;
         
          // compute the min max using the Interval computation.
          hkl::Interval i(min/2., max/2.);
          i.sin();
          min = 2 * constant::physic::tau / lambda * i.get_min();
          max = 2 * constant::physic::tau / lambda * i.get_max();

          // compute the new current value
          double const & theta = _tth->get_current().get_value() / 2.;
          double const & theta_c = _tth->get_consign().get_value() / 2.;
          double current = 2 * constant::physic::tau * sin(theta) / lambda;
          double consign = 2 * constant::physic::tau * sin(theta_c) / lambda;
          this->set_pseudoAxe(_q2th, min, current, consign, max);

          // update the writability
          double const & omega_c = _omega->get_consign().get_value();
          if (_initialized && fabs(omega_c - _omega0 - (theta_c - _tth0 / 2.)) < constant::math::epsilon)
              _writable = true;
          else
              _writable = false;
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
    double lambda = _geometry.get_source().get_waveLength().get_value();

    double tth = 2 * asin(_q2th->get_consign().get_value() * lambda / (2 * constant::physic::tau));
    double omega = _omega0 + (tth - _tth0) / 2.;

    Q2th::unconnect();
    _omega->set_consign(omega);
    _tth->set_consign(tth);
    Q2th::connect();
    Q2th::update();
  // Bouml preserved body end 00032382
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
      flux << " " << _omega0;
      flux << " " << _tth0;
      
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
      flux >> _omega0 >> _tth0;
      
      return flux;
  // Bouml preserved body end 00032482
}

Q::Q(hkl::twoC::vertical::Geometry & geometry) :
  PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>(geometry, true, true, true),
  _tth(geometry.tth())
{
  // Bouml preserved body begin 00032502
      // add all the PseudoAxes
      _q = new PseudoAxe( "q", "domega = 1/2 * d2theta.", this);
      _pseudoAxes.push_back(_q);
      
      // add observer to observable
      _tth->add_observer(this);
      
      // fill the relatedAxes
      _relatedAxes.push_back(_tth);
      
      Q::connect();
      Q::update();
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
  // Bouml preserved body end 00032602
}

void Q::update() 
{
  // Bouml preserved body begin 00032682
      if (_connected)
        {
          double lambda = _geometry.get_source().get_waveLength().get_value();

          // compute the min and max of the PseudoAxe
          hkl::Interval i(_tth->get_min().get_value() / 2., _tth->get_max().get_value() / 2.);
          i.sin();
          double min = 2 * constant::physic::tau * i.get_min() / lambda;
          double max = 2 * constant::physic::tau * i.get_max() / lambda;

          // compute the current and consign values of the PseudoAxe.
          double theta = _tth->get_current().get_value() / 2.;
          double current = 2 * constant::physic::tau * sin(theta) / lambda;

          double theta_c = _tth->get_consign().get_value() / 2.;
          double consign = 2 * constant::physic::tau * sin(theta_c) / lambda;
          this->set_pseudoAxe(_q, min, current, consign, max);

          // no need to compute the writability of the pseudoAxe.
          // As it is always writable.
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
    double lambda = _geometry.get_source().get_waveLength().get_value();
    double tth = 2 * asin(_q->get_consign().get_value() * lambda / (2 * constant::physic::tau));

    Q::unconnect();
    _tth->set_consign(tth);
    Q::connect();
    Q::update();
  // Bouml preserved body end 00032702
}

/**
 * @brief Un-Initialize the pseudoAxe.
 * This method must be call to un-initialize a pseudoAxe.
 */
void Q::uninitialize() 
{
  // Bouml preserved body begin 00041502
  // Bouml preserved body end 00041502
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
      
      return flux;
  // Bouml preserved body end 00032802
}


} // namespace hkl::twoC::vertical::pseudoAxeEngine

} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
