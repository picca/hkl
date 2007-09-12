
#include "eulerian4C_vertical_geometry.h"
#include "twoC_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      /**
       *  @brief Default constructor
       */
      Geometry::Geometry() :
          hkl::Geometry("Eulerian 4 circles", "The LPS (Orsay) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        // add the sample holder
        hkl::Holder * holder = _holders.add();
        _omega = holder->add_rotation("omega", svector(0., -1., 0.));
        _chi = holder->add_rotation("chi", svector(1, 0., 0.));
        _phi = holder->add_rotation("phi", svector(0., -1., 0.));

        // add the detector holder;
        holder = _holders.add();
        _tth = holder->add_rotation("tth", svector(0., -1., 0.));
      }

      /**
       *  @brief Another constructor.
       *  @param omega the first angle value.
       *  @param chi the second angle value.
       *  @param phi the third angle value.
       *  @param tth the fourth angle value.
       */
      Geometry::Geometry(double omega, double chi, double phi, double tth) :
          hkl::Geometry("Eulerian 4 circles", "The LPS (Orsay) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        // add the sample holder
        hkl::Holder * holder = _holders.add();
        _omega = holder->add_rotation("omega", svector(0., -1., 0.));
        _chi = holder->add_rotation("chi", svector(1, 0., 0.));
        _phi = holder->add_rotation("phi", svector(0., -1., 0.));

        // add the detector holder;
        holder = _holders.add();
        _tth = holder->add_rotation("tth", svector(0., -1., 0.));

        this->set_angles(omega, chi,phi, tth);
        this->set_angles_consign(omega, chi,phi, tth);
      }

      Geometry::~Geometry()
      {
      }

      /**
       * @brief Copy Constructor.
       */
      Geometry::Geometry(const hkl::eulerian4C::vertical::Geometry & geometry) :
          hkl::Geometry(geometry)
      {
        _omega = static_cast<hkl::axe::Rotation *>(_holders.axes()["omega"]);
        _chi = static_cast<hkl::axe::Rotation *>(_holders.axes()["chi"]);
        _phi = static_cast<hkl::axe::Rotation *>(_holders.axes()["phi"]);
        _tth = static_cast<hkl::axe::Rotation *>(_holders.axes()["tth"]);
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
       * @brief Get the _tth Axe.
       * @return A pointer on the _tth Axe.
       */
      hkl::axe::Rotation * Geometry::tth()
      {
        return _tth;
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
       * @brief Get the _tth Axe.
       * @return A pointer on the _tth Axe.
       */
      const hkl::axe::Rotation * Geometry::tth() const
        {
          return _tth;
        }

      /**
       * @brief Set the angles of the eulerian4CD::Vertical geometry.
       * @param omega The value of the "omega" Axe.
       * @param chi The value of the "chi" Axe.
       * @param phi The value of the "phi" Axe.
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles(double omega, double chi, double phi, double tth)
      {
        _omega->set_current(omega);
        _chi->set_current(chi);
        _phi->set_current(phi);
        _tth->set_current(tth);
      }

      /**
       * @brief Set the angles of the eulerian4CD::Vertical geometry.
       * @param omega The value of the "omega" Axe.
       * @param chi The value of the "chi" Axe.
       * @param phi The value of the "phi" Axe.
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles_consign(double omega, double chi, double phi, double tth)
      {
        _omega->set_consign(omega);
        _chi->set_consign(chi);
        _phi->set_consign(phi);
        _tth->set_consign(tth);
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
            _chi->set_current(0);
            _phi->set_current(0);

            _chi->set_consign(0);
            _phi->set_consign(0);
          }
        _omega->set_current(geometry.omega()->get_current());
        _tth->set_current(geometry.tth()->get_current());

        _omega->set_consign(geometry.omega()->get_consign());
        _tth->set_consign(geometry.tth()->get_consign());
      }

      /**
       * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
       * @param geometry The hkl::kappa4C::vertical::Geometry.
       * @param strict false or true if we must not care of the strictness of the conversion.
       * @throw HKLException
       */
      void Geometry::setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
      {
        // update the source
        _source = geometry.get_source();

        double const & alpha = geometry.get_alpha();

        // the current part
        double const & komega = geometry.komega()->get_current().get_value();
        double const & kappa = geometry.kappa()->get_current().get_value();
        double const & kphi = geometry.kphi()->get_current().get_value();
        double omega, chi, phi;
        hkl::kappa4C::vertical::kappa_to_eulerian(komega, kappa, kphi, alpha, omega, chi, phi);

        _omega->set_current(omega);
        _chi->set_current(chi);
        _phi->set_current(phi);
        _tth->set_current(geometry.tth()->get_current());

        // the consign part
        double const & komega_c = geometry.komega()->get_consign().get_value();
        double const & kappa_c = geometry.kappa()->get_consign().get_value();
        double const & kphi_c = geometry.kphi()->get_consign().get_value();
        hkl::kappa4C::vertical::kappa_to_eulerian(komega_c, kappa_c, kphi_c, alpha, omega, chi, phi);

        _omega->set_consign(omega);
        _chi->set_consign(chi);
        _phi->set_consign(phi);
        _tth->set_consign(geometry.tth()->get_consign());
      }

      /**
       * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
       * @param geometry The hkl::eulerian6C::Geometry.
       * @param strict false or true if we must not care of the strictness of the conversion.
       * @throw HKLException
       */
      void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException)
      {
        if (strict)
          {
            if (geometry.gamma()->get_current() != 0
                || geometry.mu()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                             "\"gamma\" = \"mu\" current values must be set to zero");
              }
            else
              {
                if (geometry.gamma()->get_consign() != 0
                    || geometry.mu()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                                 "\"gamma\" = \"mu\" consign values must be set to zero");
                  }
              }
          }
        _source = geometry.get_source();

        _omega->set_current(geometry.omega()->get_current());
        _chi->set_current(geometry.chi()->get_current());
        _phi->set_current(geometry.phi()->get_current());
        _tth->set_current(geometry.delta()->get_current());

        _omega->set_consign(geometry.omega()->get_consign());
        _chi->set_consign(geometry.chi()->get_consign());
        _phi->set_consign(geometry.phi()->get_consign());
        _tth->set_consign(geometry.delta()->get_consign());
      }

      /**
       * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
       * @param geometry The hkl::kappa6C::Geometry.
       * @param strict false or true if we must not care of the strictness of the conversion.
       * @throw HKLException
       */
      void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException)
      {
        if (strict)
          {
            if (geometry.gamma()->get_current() != 0
                || geometry.mu()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                             "\"gamma\" = \"mu\" current values must be set to zero");
              }
            else
              {
                if (geometry.gamma()->get_consign() != 0
                    || geometry.mu()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                                 "\"gamma\" = \"mu\" consign values must be set to zero");
                  }
              }
          }
        _source = geometry.get_source();

        // update the current values
        double const & alpha = geometry.get_alpha();
        double const & komega = geometry.komega()->get_current().get_value();
        double const & kappa = geometry.kappa()->get_current().get_value();
        double const & kphi = geometry.kphi()->get_current().get_value();
        double omega, chi, phi;
        hkl::kappa4C::vertical::kappa_to_eulerian(komega, kappa, kphi, alpha, omega, chi, phi);

        _omega->set_current(omega);
        _chi->set_current(chi);
        _phi->set_current(phi);
        _tth->set_current(geometry.delta()->get_current());

        // update the consign values
        double const & komega_c = geometry.komega()->get_consign().get_value();
        double const & kappa_c = geometry.kappa()->get_consign().get_value();
        double const & kphi_c = geometry.kphi()->get_consign().get_value();
        hkl::kappa4C::vertical::kappa_to_eulerian(komega_c, kappa_c, kphi_c, alpha, omega, chi, phi);

        _omega->set_consign(omega);
        _chi->set_consign(chi);
        _phi->set_consign(phi);
        _tth->set_consign(geometry.delta()->get_consign());
      }


    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
