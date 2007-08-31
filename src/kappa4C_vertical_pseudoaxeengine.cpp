
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
                                0, 1, 1);
      _parameters.add(_solution);
      
      // add all the PseudoAxes
      _omega = new PseudoAxe("omega", "omega", this);
      _chi = new PseudoAxe("chi", "chi", this);
      _phi = new PseudoAxe("phi", "phi", this);
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
      
      this->connect();
      Eulerians::update();
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
  // Bouml preserved body end 00032D82
}

void Eulerians::update() 
{
  // Bouml preserved body begin 00032E82
      if (_connected)
        {
          // compute the range of all PseudoAxes.
          /*
          // compute the current part
          double const & komega = _komega->get_current().get_value();
          double const & kappa = _kappa->get_current().get_value();
          double const & kphi = _kphi->get_current().get_value();
          double omega, chi, phi;
          hkl::kappa_to_eulerian(komega, kappa, kphi, _alpha, omega, chi, phi, _solution->get_current().get_value());
      
          // compute the consign part
          double const & komega_c = _komega->get_consign().get_value();
          double const & kappa_c = _kappa->get_consign().get_value();
          double const & kphi_c = _kphi->get_consign().get_value();
          double omega_c, chi_c, phi_c;
          hkl::kappa_to_eulerian(komega_c, kappa_c, kphi_c, _alpha, omega_c, chi_c, phi_c, _solution->get_current().get_value());
          */
          hkl::Range omega;
          hkl::Range chi;
          hkl::Range phi;
          hkl::kappa4C::vertical::kappa_to_eulerian(*_komega, *_kappa, *_kphi, _alpha, omega, chi, phi, _solution->get_current().get_value());

          this->set_pseudoAxe(_omega, omega.get_min().get_value(), omega.get_current().get_value(), omega.get_consign().get_value(), omega.get_max().get_value());
          this->set_pseudoAxe(_chi, chi.get_min().get_value(), chi.get_current().get_value(), chi.get_consign().get_value(), chi.get_max().get_value());
          this->set_pseudoAxe(_phi, phi.get_min().get_value(), phi.get_current().get_value(), phi.get_consign().get_value(), phi.get_max().get_value());
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
    double const & omega_c = _omega->get_consign().get_value();
    double const & chi_c = _chi->get_consign().get_value();
    double const & phi_c = _phi->get_consign().get_value();
    double komega_c, kappa_c, kphi_c;
    hkl::kappa4C::vertical::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c, _solution->get_current().get_value());

    Eulerians::unconnect();
    _komega->set_consign(komega_c);
    _kappa->set_consign(kappa_c);
    _kphi->set_consign(kphi_c);
    Eulerians::connect();
    Eulerians::update();
  // Bouml preserved body end 00032F02
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
      
      return flux;
  // Bouml preserved body end 00033002
}


} // namespace hkl::kappa4C::vertical::pseudoAxeEngine

} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
