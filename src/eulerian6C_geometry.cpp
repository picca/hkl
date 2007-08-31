
#include "eulerian6C_geometry.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl {

namespace eulerian6C {

/**
 *  @brief Default constructor
 */
Geometry::Geometry() :
  hkl::Geometry("Eulerian 6 circles", "A default Eulerian 6 circles diffractometer.") 
{
  // Bouml preserved body begin 0002B802
    _source.setDirection(svector(1,0,0));

    // add the sample holder
    hkl::Holder * holder = _holders.add();
    _mu = holder->add_rotation("mu", svector(0., 0., 1.));
    _omega = holder->add_rotation("omega", svector(0., -1., 0.));
    _chi = holder->add_rotation("chi", svector(1, 0., 0.));
    _phi = holder->add_rotation("phi", svector(0., -1., 0.));

    // add the detector holder;
    holder = _holders.add();
    _gamma = holder->add_rotation("gamma", svector(0., 0., 1.));
    _delta = holder->add_rotation("delta", svector(0., -1., 0.));
  // Bouml preserved body end 0002B802
}

/**
 *  @brief Another constructor.
 *  @param mu the first angle value.
 *  @param omega the second angle value.
 *  @param chi the third angle value.
 *  @param phi the fourth angle value.
 *  @param gamma the fifth angle value.
 *  @param delta the sixth angle value.
 */
Geometry::Geometry(double mu, double omega, double chi, double phi, double gamma, double delta) :
  hkl::Geometry("Eulerian 6 circles", "A default Eulerian 6 circles diffractometer.") 
{
  // Bouml preserved body begin 0002B882
    _source.setDirection(svector(1,0,0));

    // add the sample holder
    hkl::Holder * holder = _holders.add();
    _mu = holder->add_rotation("mu", svector(0., 0., 1.));
    _omega = holder->add_rotation("omega", svector(0., -1., 0.));
    _chi = holder->add_rotation("chi", svector(1, 0., 0.));
    _phi = holder->add_rotation("phi", svector(0., -1., 0.));

    // add the detector holder;
    holder = _holders.add();
    _gamma = holder->add_rotation("gamma", svector(0., 0., 1.));
    _delta = holder->add_rotation("delta", svector(0., -1., 0.));

    this->set_angles(mu, omega, chi, phi, gamma, delta);
    this->set_angles_consign(mu, omega, chi, phi, gamma, delta);
  // Bouml preserved body end 0002B882
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034382
  // Bouml preserved body end 00034382
}

/**
 * @brief Copy Constructor.
 */
Geometry::Geometry(const hkl::eulerian6C::Geometry & geometry) :
  hkl::Geometry(geometry)
{
  // Bouml preserved body begin 0002B902
    _mu = static_cast<hkl::axe::Rotation *>(_holders.axes()["mu"]);
    _omega = static_cast<hkl::axe::Rotation *>(_holders.axes()["omega"]);
    _chi = static_cast<hkl::axe::Rotation *>(_holders.axes()["chi"]);
    _phi = static_cast<hkl::axe::Rotation *>(_holders.axes()["phi"]);
    _gamma = static_cast<hkl::axe::Rotation *>(_holders.axes()["gamma"]);
    _delta = static_cast<hkl::axe::Rotation *>(_holders.axes()["delta"]);
  // Bouml preserved body end 0002B902
}

/**
 * @brief Get the _mu Axe.
 * @return A pointer on the _mu Axe.
 */
hkl::axe::Rotation * Geometry::mu() 
{
  // Bouml preserved body begin 0002C082
      return _mu;
  // Bouml preserved body end 0002C082
}

/**
 * @brief Get the _omega Axe.
 * @return A pointer on the _omega Axe.
 */
hkl::axe::Rotation * Geometry::omega() 
{
  // Bouml preserved body begin 0002BA02
      return _omega;
  // Bouml preserved body end 0002BA02
}

/**
 * @brief Get the _chi Axe.
 * @return A pointer on the _chi Axe.
 */
hkl::axe::Rotation * Geometry::chi() 
{
  // Bouml preserved body begin 0002BA82
      return _chi;
  // Bouml preserved body end 0002BA82
}

/**
 * @brief Get the _phi Axe.
 * @return A pointer on the _phi Axe.
 */
hkl::axe::Rotation * Geometry::phi() 
{
  // Bouml preserved body begin 0002BB02
      return _phi;
  // Bouml preserved body end 0002BB02
}

/**
 * @brief Get the _gamma Axe.
 * @return A pointer on the _gamma Axe.
 */
hkl::axe::Rotation * Geometry::gamma() 
{
  // Bouml preserved body begin 0002BB82
      return _gamma;
  // Bouml preserved body end 0002BB82
}

/**
 * @brief Get the _delta Axe.
 * @return A pointer on the _delta Axe.
 */
hkl::axe::Rotation * Geometry::delta() 
{
  // Bouml preserved body begin 0002C102
      return _delta;
  // Bouml preserved body end 0002C102
}

/**
 * @brief Get the _mu Axe.
 * @return A pointer on the _mu Axe.
 */
const hkl::axe::Rotation * Geometry::mu() const 
{
  // Bouml preserved body begin 0002C182
      return _mu;
  // Bouml preserved body end 0002C182
}

/**
 * @brief Get the _omega Axe.
 * @return A pointer on the _omega Axe.
 */
const hkl::axe::Rotation * Geometry::omega() const 
{
  // Bouml preserved body begin 0002BC02
      return _omega;
  // Bouml preserved body end 0002BC02
}

/**
 * @brief Get the _chi Axe.
 * @return A pointer on the _chi Axe.
 */
const hkl::axe::Rotation * Geometry::chi() const 
{
  // Bouml preserved body begin 0002BC82
      return _chi;
  // Bouml preserved body end 0002BC82
}

/**
 * @brief Get the _phi Axe.
 * @return A pointer on the _phi Axe.
 */
const hkl::axe::Rotation * Geometry::phi() const 
{
  // Bouml preserved body begin 0002BD02
      return _phi;
  // Bouml preserved body end 0002BD02
}

/**
 * @brief Get the _gamma Axe.
 * @return A pointer on the _gamma Axe.
 */
const hkl::axe::Rotation * Geometry::gamma() const 
{
  // Bouml preserved body begin 0002BD82
      return _gamma;
  // Bouml preserved body end 0002BD82
}

/**
 * @brief Get the _delta Axe.
 * @return A pointer on the _delta Axe.
 */
const hkl::axe::Rotation * Geometry::delta() const 
{
  // Bouml preserved body begin 0002C202
      return _delta;
  // Bouml preserved body end 0002C202
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param mu The value of the "omega" Axe.
 * @param omega The value of the "chi" Axe.
 * @param chi The value of the "phi" Axe.
 * @param phi The value of the "2theta" Axe.
 * @param gamma The value of the "gamma" Axe.
 * @param delta The value of the "delta" Axe.
 */
void Geometry::set_angles(double mu, double omega, double chi, double phi, double gamma, double delta) 
{
  // Bouml preserved body begin 0002BE02
    _mu->set_current(mu);
    _omega->set_current(omega);
    _chi->set_current(chi);
    _phi->set_current(phi);
    _gamma->set_current(gamma);
    _delta->set_current(delta);
  // Bouml preserved body end 0002BE02
}

/**
 * @brief Set the consign angles of the Geometry. 
 * @param mu The value of the "mu" Axe.
 * @param omega The value of the "omega" Axe.
 * @param chi The value of the "chi" Axe.
 * @param phi The value of the "phi" Axe.
 * @param gamma The value of the "gamma" Axe.
 * @param delta The value of the "delta" Axe.
 */
void Geometry::set_angles_consign(double mu, double omega, double chi, double phi, double gamma, double delta) 
{
  // Bouml preserved body begin 00040A02
    _mu->set_consign(mu);
    _omega->set_consign(omega);
    _chi->set_consign(chi);
    _phi->set_consign(phi);
    _gamma->set_consign(gamma);
    _delta->set_consign(delta);
  // Bouml preserved body end 00040A02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::twoC::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002BE82
  // update the source
    _source = geometry.get_source();

    if (strict)
      {
        _mu->set_current(0);
        _chi->set_current(0);
        _phi->set_current(0);
        _gamma->set_current(0);

        _mu->set_consign(0);
        _chi->set_consign(0);
        _phi->set_consign(0);
        _gamma->set_consign(0);
      }
    _omega->set_current(geometry.omega()->get_current());
    _delta->set_current(geometry.tth()->get_current());

    _omega->set_consign(geometry.omega()->get_consign());
    _delta->set_consign(geometry.tth()->get_consign());
  // Bouml preserved body end 0002BE82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002BF02
  // update the source
    _source = geometry.get_source();

    if (strict)
      {
        _mu->set_current(0);
        _gamma->set_current(0);

        _mu->set_consign(0);
        _gamma->set_consign(0);
      }
    _omega->set_current(geometry.omega()->get_current());
    _chi->set_current(geometry.chi()->get_current());
    _phi->set_current(geometry.phi()->get_current());
    _delta->set_current(geometry.tth()->get_current());

    _omega->set_consign(geometry.omega()->get_consign());
    _chi->set_consign(geometry.chi()->get_consign());
    _phi->set_consign(geometry.phi()->get_consign());
    _delta->set_consign(geometry.tth()->get_consign());
  // Bouml preserved body end 0002BF02
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002BF82
    double const & alpha = geometry.get_alpha();

    // compute the current values
    double const & komega = geometry.komega()->get_current().get_value();
    double const & kappa = geometry.kappa()->get_current().get_value();
    double const & kphi = geometry.kphi()->get_current().get_value();
    double omega, chi, phi;
    // this line can send an Exception so the source is updated after checking that all conversions are ok.
    hkl::kappa4C::vertical::kappa_to_eulerian(komega, kappa, kphi, alpha, omega, chi, phi);

    // compute the consign values
    double const & komega_c = geometry.komega()->get_consign().get_value();
    double const & kappa_c = geometry.kappa()->get_consign().get_value();
    double const & kphi_c = geometry.kphi()->get_consign().get_value();
    double omega_c, chi_c, phi_c;
    // this line can send an Exception so the source is updated after checking that all conversions are ok.
    hkl::kappa4C::vertical::kappa_to_eulerian(komega_c, kappa_c, kphi_c, alpha, omega_c, chi_c, phi_c);

    // update the source
    _source = geometry.get_source();

    if (strict)
      {
        _mu->set_current(0);
        _gamma->set_current(0);

        _mu->set_consign(0);
        _gamma->set_consign(0);
      }
    _omega->set_current(omega);
    _chi->set_current(chi);
    _phi->set_current(phi);
    _delta->set_current(geometry.tth()->get_current());

    _omega->set_consign(omega_c);
    _chi->set_consign(chi_c);
    _phi->set_consign(phi_c);
    _delta->set_consign(geometry.tth()->get_consign());
  // Bouml preserved body end 0002BF82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002C002
    double const & alpha = geometry.get_alpha();
    double const & komega = geometry.komega()->get_current().get_value();
    double const & kappa = geometry.kappa()->get_current().get_value();
    double const & kphi = geometry.kphi()->get_current().get_value();
    double omega, chi, phi;
    // this line can send an Exception so the source is updated after checking that all conversions are ok.
    hkl::kappa4C::vertical::kappa_to_eulerian(komega, kappa, kphi, alpha, omega, chi, phi);

    // compute the consign values
    double const & komega_c = geometry.komega()->get_consign().get_value();
    double const & kappa_c = geometry.kappa()->get_consign().get_value();
    double const & kphi_c = geometry.kphi()->get_consign().get_value();
    double omega_c, chi_c, phi_c;
    // this line can send an Exception so the source is updated after checking that all conversions are ok.
    hkl::kappa4C::vertical::kappa_to_eulerian(komega_c, kappa_c, kphi_c, alpha, omega_c, chi_c, phi_c);

    // update the source
    _source = geometry.get_source();

    _mu->set_current(geometry.mu()->get_current());
    _omega->set_current(omega);
    _chi->set_current(chi);
    _phi->set_current(phi);
    _gamma->set_current(geometry.gamma()->get_current());
    _delta->set_current(geometry.delta()->get_current());

    _mu->set_consign(geometry.mu()->get_consign());
    _omega->set_consign(omega_c);
    _chi->set_consign(chi_c);
    _phi->set_consign(phi_c);
    _gamma->set_consign(geometry.gamma()->get_consign());
    _delta->set_consign(geometry.delta()->get_consign());
  // Bouml preserved body end 0002C002
}


} // namespace hkl::eulerian6C

} // namespace hkl
