
#include "kappa4C_vertical_geometry.h"
#include "axe_rotation.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl {

namespace kappa4C {

namespace vertical {

/**
 *  @brief Default constructor
 */
Geometry::Geometry() :
  hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", 2, 50 * constant::math::degToRad)
{
  // Bouml preserved body begin 0002AC82
      _source.setDirection(svector(1,0,0));
      
      _komega = addSampleAxe(Axe("komega", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
      _kappa = addSampleAxe(Axe("kappa", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
      _kphi = addSampleAxe(Axe("kphi", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
      _tth = addDetectorAxe(Axe("2theta", "1st detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
  // Bouml preserved body end 0002AC82
}

/**
 *  @brief Another constructor.
 *  @param komega the first angle value.
 *  @param kappa the second angle value.
 *  @param kphi the third angle value.
 *  @param tth the fourth angle value.
 */
Geometry::Geometry(double komega, double kappa, double kphi, double tth) :
  hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", 2, 50 * constant::math::degToRad)
{
  // Bouml preserved body begin 0002AD02
      _source.setDirection(svector(1,0,0));
      
      _komega = addSampleAxe(Axe("komega", "1st sample axe", -constant::math::pi, komega, constant::math::pi, svector(0., 1., 0.), -1));
      _kappa = addSampleAxe(Axe("kappa", "2nd sample axe", -constant::math::pi, kappa, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
      _kphi = addSampleAxe(Axe("kphi", "3rd sample axe", -constant::math::pi, kphi, constant::math::pi, svector(0., 1., 0.), -1));
      _tth = addDetectorAxe(Axe("2theta", "1st detector axe", -constant::math::pi, tth, constant::math::pi, svector(0., 1., 0.), -1));
  // Bouml preserved body end 0002AD02
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034202
  // Bouml preserved body end 00034202
}

/**
 * @brief Copy Constructor.
 */
Geometry::Geometry(const hkl::kappa4C::vertical::Geometry & geometry) :
  hkl::geometry::Kappa(geometry)
{
  // Bouml preserved body begin 0002AD82
      _komega = &(_axes["komega"]);
      _kappa = &(_axes["kappa"]);
      _kphi = &(_axes["kphi"]);
      _tth = &(_axes["2theta"]);
  // Bouml preserved body end 0002AD82
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
hkl::axe::Rotation * Geometry::komega() 
{
  // Bouml preserved body begin 0002AE82
      return _komega;
  // Bouml preserved body end 0002AE82
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
hkl::axe::Rotation * Geometry::kappa() 
{
  // Bouml preserved body begin 0002AF02
      return _kappa;
  // Bouml preserved body end 0002AF02
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
hkl::axe::Rotation * Geometry::kphi() 
{
  // Bouml preserved body begin 0002AF82
      return _kphi;
  // Bouml preserved body end 0002AF82
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
hkl::axe::Rotation * Geometry::tth() 
{
  // Bouml preserved body begin 0002B002
      return _tth;
  // Bouml preserved body end 0002B002
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
const hkl::axe::Rotation * Geometry::komega() const 
{
  // Bouml preserved body begin 0002B082
      return _komega;
  // Bouml preserved body end 0002B082
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
const hkl::axe::Rotation * Geometry::kappa() const 
{
  // Bouml preserved body begin 0002B102
      return _kappa;
  // Bouml preserved body end 0002B102
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
const hkl::axe::Rotation * Geometry::kphi() const 
{
  // Bouml preserved body begin 0002B182
      return _kphi;
  // Bouml preserved body end 0002B182
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
const hkl::axe::Rotation * Geometry::tth() const 
{
  // Bouml preserved body begin 0002B202
      return _tth;
  // Bouml preserved body end 0002B202
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param komega The value of the "omega" Axe.
 * @param kappa The value of the "chi" Axe.
 * @param kphi The value of the "phi" Axe.
 * @param tth The value of the "2theta" Axe.
 */
void Geometry::setAngles(double komega, double kappa, double kphi, double tth) 
{
  // Bouml preserved body begin 0002B282
      _komega->set_current(komega);
      _kappa->set_current(kappa);
      _kphi->set_current(kphi);
      _tth->set_current(tth);
  // Bouml preserved body end 0002B282
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::twoC::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B302
      // update the source
      _source = geometry.get_source();
      
      if (strict)
        {
          _kappa->set_current(0);
          _kphi->set_current(0);
        }
      _komega->set_current(geometry.omega()->get_current().get_value());
      _tth->set_current(geometry.tth()->get_current().get_value());
  // Bouml preserved body end 0002B302
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B382
      // update the source
      _source = geometry.get_source();
      
      double const & chi = geometry.chi()->get_current().get_value();
      if (fabs(chi) <= 2 * _alpha)
        {
          double const & omega = geometry.omega()->get_current().get_value();
          double const & phi = geometry.phi()->get_current().get_value();
          double p = asin(tan(chi/2.)/tan(_alpha));
      
          _komega->set_current(omega + p - constant::math::pi/2.);
          _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
          _kphi->set_current(phi + p + constant::math::pi/2.);
          _tth->set_current(geometry.tth()->get_current());
        }
      else
        {
          ostringstream description;
          description << "The current E4CV \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
          HKLEXCEPTION("Can not convert geometry E4CV -> K4CV",
                       description.str());
        }
  // Bouml preserved body end 0002B382
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B402
      // update the source
      _source = geometry.get_source();
      
      double const & mu = geometry.mu()->get_current().get_value();
      double const & gamma = geometry.gamma()->get_current().get_value();
      if ((!mu && !gamma) || !strict)
        {
          double const & chi = geometry.chi()->get_current().get_value();
          if (fabs(chi) <= 2 * _alpha)
            {
              double const & omega = geometry.omega()->get_current().get_value();
              double const & phi = geometry.phi()->get_current().get_value();
              double p = asin(tan(chi/2.)/tan(_alpha));
      
              _komega->set_current(omega + p - constant::math::pi/2.);
              _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
              _kphi->set_current(phi + p + constant::math::pi/2.);
              _tth->set_current(geometry.delta()->get_current());
            }
          else
            {
              ostringstream description;
              description << "The current E6C \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
              HKLEXCEPTION("Can not convert geometry E6C -> K4CV",
                           description.str());
            }
        }
      else
        {
          ostringstream description;
          if (mu && gamma)
            {
              description << "the current E6C \"mu\" (" << mu * constant::math::radToDeg << "°) and \"gamma\" (" << gamma * constant::math::radToDeg << "°) axes must be set to zero";
            }
          else if (mu)
            {
              description << "the current E6C \"mu\" (" << mu * constant::math::radToDeg << "°) must be set to zero";
            }
          else if (gamma)
            {
              description << "the current E6C \"gamma\" (" << gamma * constant::math::radToDeg << "°) must be set to zero";
            }
          HKLEXCEPTION("Can not convert geometry E6C -> K4CV",
                       description.str());
        }
  // Bouml preserved body end 0002B402
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B482
      // update the source
      _source = geometry.get_source();
      
      double const & mu = geometry.mu()->get_current().get_value();
      double const & gamma = geometry.gamma()->get_current().get_value();
      if ((!mu && !gamma) || !strict)
        {
          _komega->set_current(geometry.komega()->get_current().get_value());
          _kappa->set_current(geometry.kappa()->get_current().get_value());
          _kphi->set_current(geometry.kphi()->get_current().get_value());
          _tth->set_current(geometry.delta()->get_current().get_value());
        }
      else
        {
          ostringstream description;
          if (mu && gamma)
            {
              description << "the current K6C \"mu\" (" << mu * constant::math::radToDeg << "°) and \"gamma\" (" << gamma * constant::math::radToDeg << "°) axes must be set to zero";
            }
          else if (mu)
            {
              description << "the current K6C \"mu\" (" << mu * constant::math::radToDeg << "°) must be set to zero";
            }
          else if (gamma)
            {
              description << "the current K6C \"gamma\" (" << gamma * constant::math::radToDeg << "°) must be set to zero";
            }
          HKLEXCEPTION("Can not convert geometry K6C -> K4CV",
                       description.str());
        }
  // Bouml preserved body end 0002B482
}


} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
