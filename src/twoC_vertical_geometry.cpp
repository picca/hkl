
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

      //detector holder
      hkl::Holder * detector = _holders.add();
      _tth = detector->add_rotation("tth", svector(0., -1., 0.));
      _tth->set_current(tth);
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
      // update the source
      _source = geometry.get_source();
  
      if ((fabs(geometry.chi()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.phi()->get_current().get_value()) < constant::math::epsilon) || !strict)
        {
          _omega->set_current(geometry.omega()->get_current().get_value());
          _tth->set_current(geometry.tth()->get_current().get_value());
        }
      else
          HKLEXCEPTION("\"chi\" and/or \"phi\" axe(s) are wrong",
                       "\"chi\" = \"phi\" must be set to zero");
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
      // update the source
      _source = geometry.get_source();
  
      if ((fabs(geometry.kappa()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.kphi()->get_current().get_value()) < constant::math::epsilon) || !strict)
        {
          _omega->set_current(geometry.komega()->get_current().get_value());
          _tth->set_current(geometry.tth()->get_current().get_value());
        }
      else
          HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) are wrong",
                       "\"kappa\" = \"kphi\" must be set to zero");
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
      // update the source
      _source = geometry.get_source();
  
      if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.chi()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.phi()->get_current().get_value()) < constant::math::epsilon) || !strict)
        {
          _omega->set_current(geometry.omega()->get_current().get_value());
          _tth->set_current(geometry.delta()->get_current().get_value());
        }
      else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" = \"chi\" = \"phi\" must be set to zero");
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
      // update the source
      _source = geometry.get_source();
  
      if ((fabs(geometry.gamma()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.mu()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.kappa()->get_current().get_value()) < constant::math::epsilon
           && fabs(geometry.kphi()->get_current().get_value()) < constant::math::epsilon) || !strict)
        {
          _omega->set_current(geometry.komega()->get_current());
          _tth->set_current(geometry.delta()->get_current());
        }
      else
          HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" axe(s) are wrong",
                       "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" must be set to zero");
  // Bouml preserved body end 0002AC02
}


} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
