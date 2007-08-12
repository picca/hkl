
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "axe_rotation.h"
#include "parameter.h"
#include "pseudoaxe.h"
#include "samplelist.h"

namespace hkl {

namespace eulerian4C {

namespace vertical {

namespace pseudoAxeEngine {

Psi::Psi(hkl::eulerian4C::vertical::Geometry & geometry, hkl::SampleList *& samples)  :
  PseudoAxeEngineWithSampleTemp<hkl::eulerian4C::vertical::Geometry>(geometry, samples, false, false, false),
  _omega(geometry.omega()),
  _chi(geometry.chi()),
  _phi(geometry.phi()),
  _tth(geometry.tth())
{
  // Bouml preserved body begin 00032882
      // Add the parameter
      _desorientation = new Parameter("Desorientation", "The maximum desorientation alors before the pseudoAxe is inactivated.",
                                          0 * constant::math::degToRad, 1e-2 * constant::math::degToRad, 180 * constant::math::degToRad);
      _parameters.add(_desorientation);
      
      // set the range
      double min = -constant::math::pi;
      double max = constant::math::pi;
      _psi_r.set_range(min, max);
      _psi_w.set_range(min, max);
      
      // add all the PseudoAxes
      _psi = new PseudoAxe( "psi", "psi is the angle of rotation around the Q vector", _psi_r, _psi_w, this);
      _pseudoAxes.push_back(_psi);
      
      // add observer to observable
      _omega->add_observer(this);
      _chi->add_observer(this);
      _phi->add_observer(this);
      _tth->add_observer(this);
      
      //fill the relatedAxes
      _relatedAxes.push_back(_omega);
      _relatedAxes.push_back(_chi);
      _relatedAxes.push_back(_phi);
      _relatedAxes.push_back(_tth);
      
      connect();
      Psi::update();
      
      // update the write part from the read part for the first time.
      _psi_w.set_current(_psi_r.get_current());
  // Bouml preserved body end 00032882
}

Psi::~Psi() 
{
  // Bouml preserved body begin 00032902
      delete _desorientation;
      delete _psi;
  // Bouml preserved body end 00032902
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Psi::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032982
      svector Q0 = _geometry.getQ();
      double norm2 = Q0.norm2();
      if (norm2 > constant::math::epsilon)
        {
          Q0 /= norm2;
          _Q0 = Q0;
          _qpsi0 = _geometry.getSampleQuaternion();
          _initialized = true;
          _writable = true;
          _readable = true;
          update();
          set_write_from_read();
        }
      else
        {
          ostringstream reason;
          reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the Q vector is null.";
          HKLEXCEPTION(reason.str(), "Check the wave length.");
        }
  // Bouml preserved body end 00032982
}

/**
 * @brief Un-Initialize the pseudoAxe.
 * This method must be call to un-initialize a pseudoAxe.
 */
void Psi::uninitialize() 
{
  // Bouml preserved body begin 00033F82
      _initialized = false;
      _writable = false;
      _readable = false;
  // Bouml preserved body end 00033F82
}

bool Psi::isValid() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032C02
      bool valid = false;
      if (_initialized)
        {
          svector Q(_geometry.getQ());
          double norm2 = Q.norm2();
          // check that |Q| is non-null
          if (norm2 > constant::math::epsilon)
            {
              Q /= norm2;
              if (Q.angle(_Q0) < _desorientation->get_current().get_value())
                {
                  Quaternion q(_geometry.getSampleQuaternion());
                  q *= _qpsi0.conjugate();
      
                  svector axe(q.getAxe());
                  //if axe = (0,0,0), we get back to the initial position so return true.
                  if (axe == svector())
                    valid = true;
                  else
                    {
                      double desorientation = axe.angle(_Q0);
                      double desorientation_max = _desorientation->get_current().get_value();
                      if (desorientation < desorientation_max
                          || fabs(desorientation - constant::math::pi) < desorientation_max)
                          valid = true;
                      else
                          valid = false;
                    }
                }
            }
        }
      if (valid)
        _writable = true;
      else
        {
          _writable = false;
          HKLEXCEPTION("The current geometry is not compatible with the pseudoAxe initialisation.","Please re-initialize it.");
        }
      return valid;
  // Bouml preserved body end 00032C02
}

void Psi::update() 
{
  // Bouml preserved body begin 00032A02
      if (_connected)
        {
          double min;
          double max;
          double current;
      
          _writable = false;
          _readable = false;
          if (_initialized)
            {
              // update the min max
              min = -constant::math::pi;
              max = constant::math::pi;
      
              // compute the current position
              svector Q(_geometry.getQ());
              double norm2 = Q.norm2();
      
              // check that |Q| is non-null
              if (norm2 > constant::math::epsilon)
                {
                  Quaternion qpsi(_geometry.getSampleQuaternion());
                  qpsi *= _qpsi0.conjugate();
      
                  // check that the desorientation of the Q vector (detector angles) did not changed.
                  Q /= norm2;
                  if (Q.angle(_Q0) < _desorientation->get_current().get_value())
                    {
                      // compute the axe of rotation of the sample quaternion.
                      svector axe(qpsi.getAxe());
                      //if axe = (0,0,0), we get back to the initial position so return true.
                      if (axe == svector())
                        {
                          current = 0;
                          _writable = true;
                          _readable = true;
                        }
                      else
                        {
                          // angle return a positiv number between 0 and pi
                          double desorientation = axe.angle(_Q0);
                          double desorientation_max = _desorientation->get_current().get_value();
                          if (desorientation < desorientation_max
                              || fabs(desorientation - constant::math::pi) < desorientation_max)
                            {
                              // if the sample rotation is compatible with a rotation around _Q0
                              svector psi_axe(_Q0);
                              // getAngleAndAxe update the axe so need to do a copy of _Q0 before
                              qpsi.getAngleAndAxe(current, psi_axe);
                              _writable = true;
                              _readable = true;
                            }
                        }
                    }
                }
            }
          else
            {
              min = max = current = 0;
            }
          _psi_r.set(min, current, max);
        }
  // Bouml preserved body end 00032A02
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Psi::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032A82
      if (Psi::isValid())
        {
          Quaternion q(_psi_w.get_current().get_value(), _Q0);
          q *= _qpsi0;
          smatrix M = q.asMatrix();
      
          double omega;
          double chi;
          double phi;
          double tth;
          if (fabs (M.get(0, 1)) < constant::math::epsilon
              && fabs (M.get(1, 0)) < constant::math::epsilon
              && fabs (M.get(2, 1)) < constant::math::epsilon
              && fabs (M.get(1, 2)) < constant::math::epsilon)
            {
              omega = _omega->get_current().get_value();
              if (M.get (1, 1) > 0)
                {
                  chi = 0;
                  phi = atan2(M.get(2, 0), M.get(0, 0)) - omega;
                }
              else
                {
                  chi = constant::math::pi;
                  phi = omega - atan2(M.get(2, 0), M.get(0, 0));
                }
              unconnect();
              _chi->set_current(chi);
              _phi->set_current(phi);
              connect();
              update();
            }
          else
            {
              //1st solution 0<chi<pi
              omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
              chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
              phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
              tth = _geometry.tth()->get_current().get_value();
              hkl::eulerian4C::vertical::Geometry g1(omega, chi, phi, tth);
      
              //2nd solution -pi<chi<0
              omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
              chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
              phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
              hkl::eulerian4C::vertical::Geometry g2(omega, chi, phi, tth);
      
              double d1 = _geometry.getDistance(g1);
              double d2 = _geometry.getDistance(g2);
              unconnect();
              if (d1 < d2)
                {
                  _omega->set_current(g1.omega()->get_current().get_value());
                  _chi->set_current(g1.chi()->get_current().get_value());
                  _phi->set_current(g1.phi()->get_current().get_value());
                  _tth->set_current(tth);
                }
              else
                {
                  _omega->set_current(g2.omega()->get_current().get_value());
                  _chi->set_current(g2.chi()->get_current().get_value());
                  _phi->set_current(g2.phi()->get_current().get_value());
                  _tth->set_current(tth);
                }
              connect();
              update();
              // update the read part after connection
            }
        }
  // Bouml preserved body end 00032A82
}

void Psi::set_write_from_read() 
{
  // Bouml preserved body begin 00038582
      _psi_w.set_current(_psi_r.get_current().get_value());
  // Bouml preserved body end 00038582
}

/**
 * @brief print on a stream the content of the Psi
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Psi::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00032B02
      PseudoAxeEngineTemp<hkl::eulerian4C::vertical::Geometry>::toStream(flux);
      _Q0.toStream(flux);
      _qpsi0.toStream(flux);
      _psi_r.toStream(flux);
      _psi_w.toStream(flux);
      _desorientation->toStream(flux);
      
      return flux;
  // Bouml preserved body end 00032B02
}

/**
 * @brief restore the content of the Psi from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Psi::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00032B82
      PseudoAxeEngineTemp<hkl::eulerian4C::vertical::Geometry>::fromStream(flux);
      _Q0.fromStream(flux);
      _qpsi0.fromStream(flux);
      _psi_r.fromStream(flux);
      _psi_w.fromStream(flux);
      _desorientation->fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00032B82
}


} // namespace hkl::eulerian4C::vertical::pseudoAxeEngine

} // namespace hkl::eulerian4C::vertical

} // namespace hkl::eulerian4C

} // namespace hkl
