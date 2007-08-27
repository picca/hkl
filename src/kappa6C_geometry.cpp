
#include "kappa6C_geometry.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"

namespace hkl {

namespace kappa6C {

/**
 * @brief Default constructor
 * @param alpha The alpha angle of the kappa geometry.
 */
Geometry::Geometry(double alpha) :
  hkl::geometry::Kappa("Kappa 6 circles", "A default Kappa 6 circles diffractometer.", alpha)  
{
  // Bouml preserved body begin 0002C282
      _source.setDirection(svector(1,0,0));
     
      // add the sample holder
      hkl::Holder * holder = _holders.add();
      _mu = holder->add_rotation("mu", hkl::svector(0., 0., 1.));
      _komega = holder->add_rotation("komega", hkl::svector(0., -1., 0.));
      _kappa = holder->add_rotation("kappa", hkl::svector(0., -cos(_alpha), -sin(_alpha)));
      _kphi = holder->add_rotation("kphi", hkl::svector(0., -1., 0.));

      // add the detector holder
      holder = _holders.add();
      _gamma = holder->add_rotation("gamma", hkl::svector(0., 0., 1.));
      _delta = holder->add_rotation("delta", hkl::svector(0., -1., 0.));
  // Bouml preserved body end 0002C282
}

/**
 * @brief Another constructor.
 * @param alpha the alpha parameter of the kappa geometry.
 * @param mu the first angle value.
 * @param komega the second angle value.
 * @param kappa the third angle value.
 * @param kphi the fourth angle value.
 * @param gamma the fifth angle value.
 * @param delta the sixth angle value.
 */
Geometry::Geometry(double alpha, double mu, double komega, double kappa, double kphi, double gamma, double delta) :
  hkl::geometry::Kappa("Kappa 6 circles", "A default Kappa 6 circles diffractometer.", alpha)  
{
  // Bouml preserved body begin 0002C302
      _source.setDirection(svector(1,0,0));
     
      // add the sample holder
      hkl::Holder * holder = _holders.add();
      _mu = holder->add_rotation("mu", hkl::svector(0., 0., 1.));
      _komega = holder->add_rotation("komega", hkl::svector(0., -1., 0.));
      _kappa = holder->add_rotation("kappa", hkl::svector(0., -cos(_alpha), -sin(_alpha)));
      _kphi = holder->add_rotation("kphi", hkl::svector(0., -1., 0.));

      // add the detector holder
      holder = _holders.add();
      _gamma = holder->add_rotation("gamma", hkl::svector(0., 0., 1.));
      _delta = holder->add_rotation("delta", hkl::svector(0., -1., 0.));

      this->set_angles(mu, komega, kappa, kphi, gamma, delta);
      this->set_angles_consign(mu, komega, kappa, kphi, gamma, delta);
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
      _mu = static_cast<hkl::axe::Rotation *>(_holders.axes()["mu"]);
      _komega = static_cast<hkl::axe::Rotation *>(_holders.axes()["komega"]);
      _kappa = static_cast<hkl::axe::Rotation *>(_holders.axes()["kappa"]);
      _kphi = static_cast<hkl::axe::Rotation *>(_holders.axes()["kphi"]);
      _gamma = static_cast<hkl::axe::Rotation *>(_holders.axes()["gamma"]);
      _delta = static_cast<hkl::axe::Rotation *>(_holders.axes()["delta"]);
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
 * @param mu The value of the "mu" Axe.
 * @param komega The value of the "komega" Axe.
 * @param kappa The value of the "kappa" Axe.
 * @param kphi The value of the "kphi" Axe.
 * @param gamma The value of the "gamma" Axe.
 * @param delta The value of the "delta" Axe.
 */
void Geometry::set_angles(double mu, double komega, double kappa, double kphi, double gamma, double delta) 
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
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param mu The value of the "mu" Axe.
 * @param komega The value of the "komega" Axe.
 * @param kappa The value of the "kappa" Axe.
 * @param kphi The value of the "kphi" Axe.
 * @param gamma The value of the "gamma" Axe.
 * @param delta The value of the "delta" Axe.
 */
void Geometry::set_angles_consign(double mu, double komega, double kappa, double kphi, double gamma, double delta) 
{
  // Bouml preserved body begin 00040C02
    _mu->set_consign(mu);
    _komega->set_consign(komega);
    _kappa->set_consign(kappa);
    _kphi->set_consign(kphi);
    _gamma->set_consign(gamma);
    _delta->set_consign(delta);
  // Bouml preserved body end 00040C02
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

        _mu->set_consign(0);
        _gamma->set_consign(0);
        _kappa->set_consign(0);
        _kphi->set_consign(0);
      }
    _komega->set_current(geometry.omega()->get_current());
    _delta->set_current(geometry.tth()->get_current());

    _komega->set_consign(geometry.omega()->get_consign());
    _delta->set_consign(geometry.tth()->get_consign());
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
    double const & omega = geometry.omega()->get_current().get_value();
    double const & chi = geometry.chi()->get_current().get_value();
    double const & phi = geometry.phi()->get_current().get_value();
    double komega, kappa, kphi;
    // the next line can throw an exception so the geometry modification must me deport after.
    hkl::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi); 

    double const & omega_c = geometry.omega()->get_current().get_value();
    double const & chi_c = geometry.chi()->get_current().get_value();
    double const & phi_c = geometry.phi()->get_current().get_value();
    double komega_c, kappa_c, kphi_c;
    // the next line can throw an exception so the geometry modification must me deport after.
    hkl::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c); 

    // update the source
    _source = geometry.get_source();

    if (strict)
      {
        _mu->set_current(0);
        _gamma->set_current(0);

        _mu->set_consign(0);
        _gamma->set_consign(0);
      }
    _komega->set_current(komega);
    _kappa->set_current(kappa);
    _kphi->set_current(kphi);
    _delta->set_current(geometry.tth()->get_current());

    _komega->set_consign(komega_c);
    _kappa->set_consign(kappa_c);
    _kphi->set_consign(kphi_c);
    _delta->set_consign(geometry.tth()->get_consign());
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

        _mu->set_consign(0);
        _gamma->set_consign(0);
      }
    _komega->set_current(geometry.komega()->get_current());
    _kappa->set_current(geometry.kappa()->get_current());
    _kphi->set_current(geometry.kphi()->get_current());
    _delta->set_current(geometry.tth()->get_current());

    _komega->set_consign(geometry.komega()->get_consign());
    _kappa->set_consign(geometry.kappa()->get_consign());
    _kphi->set_consign(geometry.kphi()->get_consign());
    _delta->set_consign(geometry.tth()->get_consign());
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
      double const & omega = geometry.omega()->get_current().get_value();
      double const & phi = geometry.phi()->get_current().get_value();
      double const & chi = geometry.chi()->get_current().get_value();
      double komega, kappa, kphi;
      hkl::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi); 

      double const & omega_c = geometry.omega()->get_consign().get_value();
      double const & phi_c = geometry.phi()->get_consign().get_value();
      double const & chi_c = geometry.chi()->get_consign().get_value();
      double komega_c, kappa_c, kphi_c;
      hkl::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c); 

      // update the source
      _source = geometry.get_source();

      _mu->set_current(geometry.mu()->get_current());
      _komega->set_current(komega);
      _kappa->set_current(kappa);
      _kphi->set_current(kphi);
      _gamma->set_current(geometry.gamma()->get_current());
      _delta->set_current(geometry.delta()->get_current());

      _mu->set_consign(geometry.mu()->get_consign());
      _komega->set_consign(komega_c);
      _kappa->set_consign(kappa_c);
      _kphi->set_consign(kphi_c);
      _gamma->set_consign(geometry.gamma()->get_consign());
      _delta->set_consign(geometry.delta()->get_consign());
  // Bouml preserved body end 0002CC82
}


} // namespace hkl::kappa6C

} // namespace hkl
