
#include "kappa4C_vertical_pseudoaxeengine.h"
#include "axe_rotation.h"
#include "parameter.h"
#include "pseudoaxe.h"

namespace hkl {

namespace kappa4C {

namespace vertical {

namespace pseudoAxeEngine {

Eulerians::Eulerians(hkl::kappa4C::vertical::Geometry & geometry) :
  hkl::PseudoAxeEngineTemp<hkl::kappa4C::vertical::Geometry>(geometry, true, true, true),
  _alpha(geometry.get_alpha()),
  _komega(geometry.komega()),
  _kappa(geometry.kappa()),
  _kphi(geometry.kphi())
{
  // Bouml preserved body begin 00032C82
      // parameters
      _solution = new Parameter("solution", "Switch between solution 0 or 1(default)\n",
                                0, 0, 1);
      _parameters.add(_solution);
      
      // set the ranges
      _omega_r.set_range(-constant::math::pi, constant::math::pi);
      _omega_w.set_range(-constant::math::pi, constant::math::pi);
      _chi_r.set_range(-_alpha * 2., _alpha * 2.);
      _chi_w.set_range(-_alpha * 2., _alpha * 2.);
      _phi_r.set_range(-constant::math::pi, constant::math::pi);
      _phi_w.set_range(-constant::math::pi, constant::math::pi);
      
      // add all the PseudoAxes
      _omega = new PseudoAxe("omega", "omega", _omega_r, _omega_w, this);
      _chi = new PseudoAxe("chi", "chi", _chi_r, _chi_w, this);
      _phi = new PseudoAxe("phi", "phi", _phi_r, _phi_w, this);
      _pseudoAxes.push_back(_omega);
      _pseudoAxes.push_back(_chi);
      _pseudoAxes.push_back(_phi);
      
      // add observer to observable
      _komega->add_observer(this);
      _kappa->add_observer(this);
      _kphi->add_observer(this);
      
      // fill relatedAxes
      _relatedAxes.push_back(_komega);
      _relatedAxes.push_back(_kappa);
      _relatedAxes.push_back(_kphi);
      
      connect();
      Eulerians::update();
      
      // update the write part from the read part for the first time.
      _omega_w.set_current(_omega_r.get_current());
      _chi_w.set_current(_chi_r.get_current());
      _phi_w.set_current(_phi_r.get_current());
  // Bouml preserved body end 00032C82
}

Eulerians::~Eulerians() 
{
  // Bouml preserved body begin 00032D02
      delete _solution;
      
      delete _omega;
      delete _chi;
      delete _phi;
  // Bouml preserved body end 00032D02
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
void Eulerians::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032D82
      _initialized = true;
      _writable = true;
      set_write_from_read();
  // Bouml preserved body end 00032D82
}

void Eulerians::update() 
{
  // Bouml preserved body begin 00032E82
      if (_connected)
        {
          double const & komega = _komega->get_current().get_value();
          double const & kappa = _kappa->get_current().get_value();
          double const & kphi = _kphi->get_current().get_value();
          double omega, chi, phi;
          hkl::kappa_to_eulerian(komega, kappa, kphi, _alpha, omega, chi, phi, _solution->get_current().get_value());
      
          _omega_r.set_current(omega);
          _chi_r.set_current(chi);
          _phi_r.set_current(phi);
        }
  // Bouml preserved body end 00032E82
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
void Eulerians::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00032F02
      if (_initialized)
        {
          double const & omega = _omega_w.get_current().get_value();
          double const & chi = _chi_w.get_current().get_value();
          double const & phi = _phi_w.get_current().get_value();
          double komega, kappa, kphi;
          hkl::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi);

          Eulerians::unconnect();
          _komega->set_current(komega);
          _kappa->set_current(kappa);
          _kphi->set_current(kphi);
          Eulerians::connect();
          Eulerians::update();
        }
      else
        {
          HKLEXCEPTION("Can not write on un uninitialized pseudoAxe", "Please initialize it.");
        }
  // Bouml preserved body end 00032F02
}

void Eulerians::set_write_from_read() 
{
  // Bouml preserved body begin 00038602
      _omega_w.set_current(_omega_r.get_current().get_value());
      _chi_w.set_current(_chi_r.get_current().get_value());
      _phi_w.set_current(_phi_r.get_current().get_value());
  // Bouml preserved body end 00038602
}

/**
 * @brief print on a stream the content of the Eulerians
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Eulerians::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00032F82
      ((hkl::PseudoAxeEngineTemp<hkl::kappa4C::vertical::Geometry> *)this)->toStream(flux);
      _solution->toStream(flux);
      _omega_r.toStream(flux);
      _omega_w.toStream(flux);
      _chi_r.toStream(flux);
      _chi_w.toStream(flux);
      _phi_r.toStream(flux);
      _phi_w.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00032F82
}

/**
 * @brief restore the content of the Eulerians from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Eulerians::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00033002
      ((hkl::PseudoAxeEngineTemp<hkl::kappa4C::vertical::Geometry> *)this)->fromStream(flux);
      _solution->fromStream(flux);
      _omega_r.fromStream(flux);
      _omega_w.fromStream(flux);
      _chi_r.fromStream(flux);
      _chi_w.fromStream(flux);
      _phi_r.fromStream(flux);
      _phi_w.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00033002
}


} // namespace hkl::kappa4C::vertical::pseudoAxeEngine

} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
