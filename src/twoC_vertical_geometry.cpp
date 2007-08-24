
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl {

namespace twoC {

namespace vertical {

/**
 *  @brief Default constructor
 */
Geometry::Geometry() :
  hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")  
{
  // Bouml preserved body begin 0002A402
      _source.setDirection(svector(1,0,0));
  
      // sample holder
      hkl::Holder * sample = _holders.add();
      _omega = sample->add_rotation("omega", svector(0., -1., 0.));

      //detector holder
      hkl::Holder * detector = _holders.add();
      _tth = detector->add_rotation("tth", svector(0., -1., 0.));
  // Bouml preserved body end 0002A402
}

/**
 * @brief Another constructor.
 * @param omega the first angle value.
 * @param tth the second angle value.
 */
Geometry::Geometry(double omega, double tth) :
  hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")  
{
  // Bouml preserved body begin 0002A482
      _source.setDirection(svector(1,0,0));
  
      // sample holder
      hkl::Holder * sample = _holders.add();
      _omega = sample->add_rotation("omega", svector(0., -1., 0.));
      _omega->set_current(omega);
      _omega->set_consign(omega);

      //detector holder
      hkl::Holder * detector = _holders.add();
      _tth = detector->add_rotation("tth", svector(0., -1., 0.));
      _tth->set_current(tth);
      _tth->set_consign(tth);
  // Bouml preserved body end 0002A482
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034302
  // Bouml preserved body end 00034302
}

/**
 * @brief Copy Constructor.
 */
Geometry::Geometry(const hkl::twoC::vertical::Geometry & geometry) :
  hkl::Geometry(geometry)
{
  // Bouml preserved body begin 0002A502
  _omega = static_cast<hkl::axe::Rotation *>(_holders.axes()["omega"]);
  _tth = static_cast<hkl::axe::Rotation *>(_holders.axes()["tth"]);
  // Bouml preserved body end 0002A502
}

/**
 * @brief Get the _omega Axe.
 * @return A pointer on the _omega Axe.
 */
hkl::axe::Rotation * Geometry::omega() 
{
  // Bouml preserved body begin 0002A602
      return _omega;
  // Bouml preserved body end 0002A602
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
hkl::axe::Rotation * Geometry::tth() 
{
  // Bouml preserved body begin 0002A782
      return _tth;
  // Bouml preserved body end 0002A782
}

/**
 * @brief Get the _omega Axe.
 * @return A pointer on the _omega Axe.
 */
const hkl::axe::Rotation * Geometry::omega() const 
{
  // Bouml preserved body begin 0002A802
      return _omega;
  // Bouml preserved body end 0002A802
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
const hkl::axe::Rotation * Geometry::tth() const 
{
  // Bouml preserved body begin 0002A982
      return _tth;
  // Bouml preserved body end 0002A982
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param omega The value of the "omega" Axe.
 * @param tth The value of the "2theta" Axe.
 */
void Geometry::setAngles(double omega, double tth) 
{
  // Bouml preserved body begin 0002AA02
      _omega->set_current(omega);
      _tth->set_current(tth);
  // Bouml preserved body end 0002AA02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002AA82
  // check that chi and phi current and consign values are compatible with the convertion in case of a strict conversion
    if (strict)
      {
        // first the current value
        if (geometry.chi()->get_current() != 0 || geometry.phi()->get_current() != 0)
          {
            HKLEXCEPTION("\"chi\" and/or \"phi\" current values are wrong",
                         "\"chi\" = \"phi\" current values must be set to zero");
          }
        else
          {
            // the the consign values
            if (geometry.chi()->get_consign() != 0 || geometry.phi()->get_consign() != 0)
              {
                HKLEXCEPTION("\"chi\" and/or \"phi\" consign values are wrong",
                             "\"chi\" = \"phi\" consign values must be set to zero");
              }
          }
      }
    // everything ok so we can set the Geometry.
    _source = geometry.get_source();

    _omega->set_current(geometry.omega()->get_current().get_value());
    _tth->set_current(geometry.tth()->get_current().get_value());

    _omega->set_consign(geometry.omega()->get_consign().get_value());
    _tth->set_consign(geometry.tth()->get_consign().get_value());
  // Bouml preserved body end 0002AA82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002AB02
  // check that kappa and kphi current and consign values are compatible with the convertion in case of a strict conversion
    if (strict)
      {
        // first the current value
        if (geometry.kappa()->get_current() != 0 || geometry.kphi()->get_current() != 0)
          {
            HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) current values are wrong",
                         "\"kappa\" = \"kphi\" current values must be set to zero");
          }
        else
          {
            // the the consign values
            if (geometry.kappa()->get_consign() != 0 || geometry.kphi()->get_consign() != 0)
              {
                HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) consign values are wrong",
                             "\"kappa\" = \"kphi\" consign values must be set to zero");
              }
          }
      }
    // everything ok so we can set the Geometry.
    _source = geometry.get_source();

    _omega->set_current(geometry.komega()->get_current().get_value());
    _tth->set_current(geometry.tth()->get_current().get_value());

    _omega->set_consign(geometry.komega()->get_consign().get_value());
    _tth->set_consign(geometry.tth()->get_consign().get_value());
  // Bouml preserved body end 0002AB02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
    // Bouml preserved body begin 0002AB82
    // check that gamma, mu, chi and phi current and consign values are compatible with the convertion
    if (strict)
      {
        // first the current value
        if (geometry.gamma()->get_current() != 0
            || geometry.mu()->get_current() != 0
            || geometry.chi()->get_current() != 0
            || geometry.phi()->get_current() != 0)
          {
            HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" current values are wrong",
                         "\"gamma\" = \"mu\" = \"chi\" = \"phi\" current values must be set to zero");
          }
        else
          {
            // the the consign values
            if (geometry.gamma()->get_consign() != 0
                || geometry.mu()->get_consign() != 0
                || geometry.chi()->get_consign() != 0
                || geometry.phi()->get_consign() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" consign values are wrong",
                             "\"gamma\" = \"mu\" = \"chi\" = \"phi\" consign values must be set to zero");
              }
          }
      }
    // ok so set the Geometry
    _source = geometry.get_source();

    _omega->set_current(geometry.omega()->get_current().get_value());
    _tth->set_current(geometry.delta()->get_current().get_value());

    _omega->set_consign(geometry.omega()->get_consign().get_value());
    _tth->set_consign(geometry.delta()->get_consign().get_value());
    // Bouml preserved body end 0002AB82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002AC02
    // check that gamma, mu, kappa and kphi current and consign values are compatible with the convertion
    if(strict)
      {
        // first the current value
        if (geometry.gamma()->get_current() != 0
            || geometry.mu()->get_current() != 0
            || geometry.kappa()->get_current() != 0
            || geometry.kphi()->get_current() != 0)
          {
            HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" current values are wrong",
                         "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" current values must be set to zero");
          }
        else
          {
            // the the consign values
            if (geometry.gamma()->get_consign() != 0
                || geometry.mu()->get_consign() != 0
                || geometry.kappa()->get_consign() != 0
                || geometry.kphi()->get_consign() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" consign values are wrong",
                             "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" consign values must be set to zero");
              }
          }
      }
    _source = geometry.get_source();

    _omega->set_current(geometry.komega()->get_current().get_value());
    _tth->set_current(geometry.delta()->get_current().get_value());

    _omega->set_consign(geometry.komega()->get_consign().get_value());
    _tth->set_consign(geometry.delta()->get_consign().get_value());
  // Bouml preserved body end 0002AC02
}


} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
