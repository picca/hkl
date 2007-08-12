
#include "kappa6C_geometry.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"

namespace hkl {

namespace kappa6C {

/**
 *  @brief Default constructor
 */
Geometry::Geometry() :
  hkl::geometry::Kappa("Kappa 6 circles", "A default Kappa 6 circles diffractometer.", 2, 50 * constant::math::degToRad) 
{
  // Bouml preserved body begin 0002C282
      _source.setDirection(svector(1,0,0));
      
      _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
      _komega = addSampleAxe(Axe("komega", "2nd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
      _kappa = addSampleAxe(Axe("kappa", "3rd sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
      _kphi = addSampleAxe(Axe("kphi", "4th sample axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
      _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 0., 1.), 1));
      _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, 0, constant::math::pi, svector(0., 1., 0.), -1));
  // Bouml preserved body end 0002C282
}

/**
 *  @brief Another constructor.
 *  @param mu the first angle value.
 *  @param komega the second angle value.
 *  @param kappa the third angle value.
 *  @param kphi the fourth angle value.
 *  @param gamma the fifth angle value.
 *  @param delta the sixth angle value.
 */
Geometry::Geometry(double mu, double komega, double kappa, double kphi, double gamma, double delta) :
  hkl::geometry::Kappa("Kappa 6 circles", "A default Kappa 6 circles diffractometer.", 2, 50 * constant::math::degToRad) 
{
  // Bouml preserved body begin 0002C302
      _source.setDirection(svector(1,0,0));
      
      _mu = addSampleAxe(Axe("mu", "1st sample axe", -constant::math::pi, mu, constant::math::pi, svector(0., 0., 1.), 1));
      _komega = addSampleAxe(Axe("komega", "2nd sample axe", -constant::math::pi, komega, constant::math::pi, svector(0., 1., 0.), -1));
      _kappa = addSampleAxe(Axe("kappa", "3rd sample axe", -constant::math::pi, kappa, constant::math::pi, svector(0., cos(_alpha), sin(_alpha)), -1));
      _kphi = addSampleAxe(Axe("kphi", "4th sample axe", -constant::math::pi, kphi, constant::math::pi, svector(0., 1., 0.), -1));
      _gamma = addDetectorAxe(Axe("gamma", "1st detector axe", -constant::math::pi, gamma, constant::math::pi, svector(0., 0., 1.), 1));
      _delta = addDetectorAxe(Axe("delta", "2nd detector axe", -constant::math::pi, delta, constant::math::pi, svector(0., 1., 0.), -1));
  // Bouml preserved body end 0002C302
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034402
  // Bouml preserved body end 00034402
}

/**
 * @brief Copy Constructor.
 */
Geometry::Geometry(const hkl::kappa6C::Geometry & geometry) :
  hkl::geometry::Kappa(geometry)
{
  // Bouml preserved body begin 0002C382
      _mu = &_axes["mu"];
      _komega = &_axes["komega"];
      _kappa = &_axes["kappa"];
      _kphi = &_axes["kphi"];
      _gamma = &_axes["gamma"];
      _delta = &_axes["delta"];
  // Bouml preserved body end 0002C382
}

/**
 * @brief Get the _mu Axe.
 * @return A pointer on the _mu Axe.
 */
hkl::axe::Rotation * Geometry::mu() 
{
  // Bouml preserved body begin 0002C482
      return _mu;
  // Bouml preserved body end 0002C482
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
hkl::axe::Rotation * Geometry::komega() 
{
  // Bouml preserved body begin 0002C502
      return _komega;
  // Bouml preserved body end 0002C502
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
hkl::axe::Rotation * Geometry::kappa() 
{
  // Bouml preserved body begin 0002C582
      return _kappa;
  // Bouml preserved body end 0002C582
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
hkl::axe::Rotation * Geometry::kphi() 
{
  // Bouml preserved body begin 0002C602
      return _kphi;
  // Bouml preserved body end 0002C602
}

/**
 * @brief Get the _gamma Axe.
 * @return A pointer on the _gamma Axe.
 */
hkl::axe::Rotation * Geometry::gamma() 
{
  // Bouml preserved body begin 0002C682
      return _gamma;
  // Bouml preserved body end 0002C682
}

/**
 * @brief Get the _delta Axe.
 * @return A pointer on the _delta Axe.
 */
hkl::axe::Rotation * Geometry::delta() 
{
  // Bouml preserved body begin 0002C702
      return _delta;
  // Bouml preserved body end 0002C702
}

/**
 * @brief Get the _mu Axe.
 * @return A pointer on the _mu Axe.
 */
const hkl::axe::Rotation * Geometry::mu() const 
{
  // Bouml preserved body begin 0002C782
      return _mu;
  // Bouml preserved body end 0002C782
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
const hkl::axe::Rotation * Geometry::komega() const 
{
  // Bouml preserved body begin 0002C802
      return _komega;
  // Bouml preserved body end 0002C802
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
const hkl::axe::Rotation * Geometry::kappa() const 
{
  // Bouml preserved body begin 0002C882
      return _kappa;
  // Bouml preserved body end 0002C882
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
const hkl::axe::Rotation * Geometry::kphi() const 
{
  // Bouml preserved body begin 0002C902
      return _kphi;
  // Bouml preserved body end 0002C902
}

/**
 * @brief Get the _gamma Axe.
 * @return A pointer on the _gamma Axe.
 */
const hkl::axe::Rotation * Geometry::gamma() const 
{
  // Bouml preserved body begin 0002C982
      return _gamma;
  // Bouml preserved body end 0002C982
}

/**
 * @brief Get the _delta Axe.
 * @return A pointer on the _delta Axe.
 */
const hkl::axe::Rotation * Geometry::delta() const 
{
  // Bouml preserved body begin 0002CA02
      return _delta;
  // Bouml preserved body end 0002CA02
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param mu The value of the "omega" Axe.
 * @param komega The value of the "chi" Axe.
 * @param kappa The value of the "phi" Axe.
 * @param kphi The value of the "2theta" Axe.
 * @param gamma The value of the "gamma" Axe.
 * @param delta The value of the "delta" Axe.
 */
void Geometry::setAngles(double mu, double komega, double kappa, double kphi, double gamma, double delta) 
{
  // Bouml preserved body begin 0002CA82
      _mu->set_current(mu);
      _komega->set_current(komega);
      _kappa->set_current(kappa);
      _kphi->set_current(kphi);
      _gamma->set_current(gamma);
      _delta->set_current(delta);
  // Bouml preserved body end 0002CA82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::twoC::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002CB02
      // update the source
      _source = geometry.get_source();
      
      if (strict)
        {
          _mu->set_current(0);
          _gamma->set_current(0);
          _kappa->set_current(0);
          _kphi->set_current(0);
        }
      _komega->set_current(geometry.omega()->get_current());
      _delta->set_current(geometry.tth()->get_current());
  // Bouml preserved body end 0002CB02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002CB82
      // update the source
      _source = geometry.get_source();
      
      double const & chi = geometry.chi()->get_current().get_value();
      if (fabs(chi) <= 2 * _alpha)
        {
          double const & omega = geometry.omega()->get_current().get_value();
          double const & phi = geometry.phi()->get_current().get_value();
          double p = asin(tan(chi/2.)/tan(_alpha));
      
          if (strict)
            {
              _mu->set_current(0);
              _gamma->set_current(0);
            }
          _komega->set_current(omega + p - constant::math::pi/2.);
          _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
          _kphi->set_current(phi + p + constant::math::pi/2.);
          _delta->set_current(geometry.tth()->get_current());
        }
      else
        {
          ostringstream description;
          description << "The current E4CV \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
          HKLEXCEPTION("Can not convert geometry E4CV -> K6C",
                       description.str());
        }
  // Bouml preserved body end 0002CB82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002CC02
      // update the source
      _source = geometry.get_source();
      
      if (strict)
        {
          _mu->set_current(0);
          _gamma->set_current(0);
        }
      _komega->set_current(geometry.komega()->get_current().get_value());
      _kappa->set_current(geometry.kappa()->get_current().get_value());
      _kphi->set_current(geometry.kphi()->get_current().get_value());
      _delta->set_current(geometry.tth()->get_current().get_value());
  // Bouml preserved body end 0002CC02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002CC82
      // update the source
      _source = geometry.get_source();
      
      double const & chi = geometry.chi()->get_current().get_value();
      if (chi <= 2 * _alpha)
        {
          double const & omega = geometry.omega()->get_current().get_value();
          double const & phi = geometry.phi()->get_current().get_value();
          double p = asin(tan(chi/2.)/tan(_alpha));
      
          _mu->set_current(geometry.mu()->get_current());
          _komega->set_current(omega + p - constant::math::pi/2.);
          _kappa->set_current(-2 * asin(sin(chi/2.)/sin(_alpha)));
          _kphi->set_current(phi + p + constant::math::pi/2.);
          _gamma->set_current(geometry.gamma()->get_current());
          _delta->set_current(geometry.delta()->get_current());
        }
      else
        {
          ostringstream description;
          description << "The current E6C \"chi\" axe (" << chi * constant::math::radToDeg << "°) must be lower than 2*alpha (" << 2*_alpha*constant::math::radToDeg << "°)";
          HKLEXCEPTION("Can not convert geometry E6C -> K6C",
                       description.str());
        }
  // Bouml preserved body end 0002CC82
}


} // namespace hkl::kappa6C

} // namespace hkl
