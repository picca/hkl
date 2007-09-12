
#include "eulerian6C_geometry.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace eulerian6C
    {

    /**
     *  @brief Default constructor
     */
    Geometry::Geometry() :
        hkl::Geometry("Eulerian 6 circles", "A default Eulerian 6 circles diffractometer.")
    {
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
    }

    Geometry::~Geometry()
    {
    }

    /**
     * @brief Copy Constructor.
     */
    Geometry::Geometry(const hkl::eulerian6C::Geometry & geometry) :
        hkl::Geometry(geometry)
    {
      _mu = static_cast<hkl::axe::Rotation *>(_holders.axes()["mu"]);
      _omega = static_cast<hkl::axe::Rotation *>(_holders.axes()["omega"]);
      _chi = static_cast<hkl::axe::Rotation *>(_holders.axes()["chi"]);
      _phi = static_cast<hkl::axe::Rotation *>(_holders.axes()["phi"]);
      _gamma = static_cast<hkl::axe::Rotation *>(_holders.axes()["gamma"]);
      _delta = static_cast<hkl::axe::Rotation *>(_holders.axes()["delta"]);
    }

    /**
     * @brief Get the _mu Axe.
     * @return A pointer on the _mu Axe.
     */
    hkl::axe::Rotation * Geometry::mu()
    {
      return _mu;
    }

    /**
     * @brief Get the _omega Axe.
     * @return A pointer on the _omega Axe.
     */
    hkl::axe::Rotation * Geometry::omega()
    {
      return _omega;
    }

    /**
     * @brief Get the _chi Axe.
     * @return A pointer on the _chi Axe.
     */
    hkl::axe::Rotation * Geometry::chi()
    {
      return _chi;
    }

    /**
     * @brief Get the _phi Axe.
     * @return A pointer on the _phi Axe.
     */
    hkl::axe::Rotation * Geometry::phi()
    {
      return _phi;
    }

    /**
     * @brief Get the _gamma Axe.
     * @return A pointer on the _gamma Axe.
     */
    hkl::axe::Rotation * Geometry::gamma()
    {
      return _gamma;
    }

    /**
     * @brief Get the _delta Axe.
     * @return A pointer on the _delta Axe.
     */
    hkl::axe::Rotation * Geometry::delta()
    {
      return _delta;
    }

    /**
     * @brief Get the _mu Axe.
     * @return A pointer on the _mu Axe.
     */
    const hkl::axe::Rotation * Geometry::mu() const
      {
        return _mu;
      }

    /**
     * @brief Get the _omega Axe.
     * @return A pointer on the _omega Axe.
     */
    const hkl::axe::Rotation * Geometry::omega() const
      {
        return _omega;
      }

    /**
     * @brief Get the _chi Axe.
     * @return A pointer on the _chi Axe.
     */
    const hkl::axe::Rotation * Geometry::chi() const
      {
        return _chi;
      }

    /**
     * @brief Get the _phi Axe.
     * @return A pointer on the _phi Axe.
     */
    const hkl::axe::Rotation * Geometry::phi() const
      {
        return _phi;
      }

    /**
     * @brief Get the _gamma Axe.
     * @return A pointer on the _gamma Axe.
     */
    const hkl::axe::Rotation * Geometry::gamma() const
      {
        return _gamma;
      }

    /**
     * @brief Get the _delta Axe.
     * @return A pointer on the _delta Axe.
     */
    const hkl::axe::Rotation * Geometry::delta() const
      {
        return _delta;
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
      _mu->set_current(mu);
      _omega->set_current(omega);
      _chi->set_current(chi);
      _phi->set_current(phi);
      _gamma->set_current(gamma);
      _delta->set_current(delta);
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
      _mu->set_consign(mu);
      _omega->set_consign(omega);
      _chi->set_consign(chi);
      _phi->set_consign(phi);
      _gamma->set_consign(gamma);
      _delta->set_consign(delta);
    }

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::twoC::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void Geometry::setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
    {
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
    }

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::eulerian4C::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
    {
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
    }

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::kappa4C::vertical::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
    {
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
    }

    /**
     * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
     * @param geometry The hkl::kappa6C::Geometry.
     * @param strict false or true if we must not care of the strictness of the conversion.
     * @throw HKLException
     */
    void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException)
    {
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
    }


  } // namespace hkl::eulerian6C

} // namespace hkl
