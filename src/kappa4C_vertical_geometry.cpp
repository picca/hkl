/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "kappa4C_vertical_geometry.h"
#include "axe_rotation.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace kappa4C
    {

    namespace vertical
      {

      /**
       * @brief Default constructor
       * @param alpha the alpha angle of the kappa geometry
       */
      Geometry::Geometry(double alpha) :
          hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", alpha)
      {
        _source.setDirection(svector(1,0,0));

        // add the sample holder
        hkl::Holder * holder = _holders.add();
        _komega = holder->add_rotation("komega", svector(0., -1., 0.));
        _kappa = holder->add_rotation("kappa", svector(0., -cos(_alpha), -sin(_alpha)));
        _kphi = holder->add_rotation("kphi", svector(0., -1., 0.));

        // add the detector holder
        holder = _holders.add();
        _tth = holder->add_rotation("tth", svector(0., -1., 0.));
      }

      /**
       * @brief Another constructor.
       * @param alpha the alpha angle of the kappa geometry.
       * @param komega the first angle value.
       * @param kappa the second angle value.
       * @param kphi the third angle value.
       * @param tth the fourth angle value.
       */
      Geometry::Geometry(double alpha, double komega, double kappa, double kphi, double tth) :
          hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", alpha)
      {
        _source.setDirection(svector(1,0,0));

        // add the sample holder
        hkl::Holder * holder = _holders.add();
        _komega = holder->add_rotation("komega", svector(0., -1., 0.));
        _kappa = holder->add_rotation("kappa", svector(0., -cos(_alpha), -sin(_alpha)));
        _kphi = holder->add_rotation("kphi", svector(0., -1., 0.));

        // add the detector holder
        holder = _holders.add();
        _tth = holder->add_rotation("tth", svector(0., -1., 0.));

        this->set_angles(komega, kappa, kphi, tth);
        this->set_angles_consign(komega, kappa, kphi, tth);
      }

      Geometry::~Geometry()
      {
      }

      /**
       * @brief Copy Constructor.
       */
      Geometry::Geometry(const hkl::kappa4C::vertical::Geometry & geometry) :
          hkl::geometry::Kappa(geometry)
      {
        _komega = static_cast<hkl::axe::Rotation *>(_holders.axes()["komega"]);
        _kappa = static_cast<hkl::axe::Rotation *>(_holders.axes()["kappa"]);
        _kphi = static_cast<hkl::axe::Rotation *>(_holders.axes()["kphi"]);
        _tth = static_cast<hkl::axe::Rotation *>(_holders.axes()["tth"]);
      }

      /**
       * @brief Get the _komega Axe.
       * @return A pointer on the _komega Axe.
       */
      hkl::axe::Rotation * Geometry::komega()
      {
        return _komega;
      }

      /**
       * @brief Get the _kappa Axe.
       * @return A pointer on the _kappa Axe.
       */
      hkl::axe::Rotation * Geometry::kappa()
      {
        return _kappa;
      }

      /**
       * @brief Get the _kphi Axe.
       * @return A pointer on the _kphi Axe.
       */
      hkl::axe::Rotation * Geometry::kphi()
      {
        return _kphi;
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
       * @brief Get the _komega Axe.
       * @return A pointer on the _komega Axe.
       */
      const hkl::axe::Rotation * Geometry::komega() const
        {
          return _komega;
        }

      /**
       * @brief Get the _kappa Axe.
       * @return A pointer on the _kappa Axe.
       */
      const hkl::axe::Rotation * Geometry::kappa() const
        {
          return _kappa;
        }

      /**
       * @brief Get the _kphi Axe.
       * @return A pointer on the _kphi Axe.
       */
      const hkl::axe::Rotation * Geometry::kphi() const
        {
          return _kphi;
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
       * @param komega The value of the "komega" Axe.
       * @param kappa The value of the "kappa" Axe.
       * @param kphi The value of the "kphi" Axe.
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles(double komega, double kappa, double kphi, double tth)
      {
        _komega->set_current(komega);
        _kappa->set_current(kappa);
        _kphi->set_current(kphi);
        _tth->set_current(tth);
      }

      /**
       * @brief Set the angles of the eulerian4CD::Vertical geometry.
       * @param komega The value of the "komega" Axe.
       * @param kappa The value of the "kappa" Axe.
       * @param kphi The value of the "kphi" Axe.
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles_consign(double komega, double kappa, double kphi, double tth)
      {
        _komega->set_consign(komega);
        _kappa->set_consign(kappa);
        _kphi->set_consign(kphi);
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
            _kappa->set_current(0);
            _kphi->set_current(0);

            _kappa->set_consign(0);
            _kphi->set_consign(0);
          }
        _komega->set_current(geometry.omega()->get_current());
        _tth->set_current(geometry.tth()->get_current());

        _komega->set_consign(geometry.omega()->get_consign());
        _tth->set_consign(geometry.tth()->get_consign());
      }

      /**
       * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
       * @param geometry The hkl::eulerian4C::vertical::Geometry.
       * @param strict false or true if we must not care of the strictness of the conversion.
       * @throw HKLException
       */
      void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
      {
        double const & omega = geometry.omega()->get_current().get_value();
        double const & chi = geometry.chi()->get_current().get_value();
        double const & phi = geometry.phi()->get_current().get_value();
        double komega, kappa, kphi;
        //this line can throw an exception so deport the source modification after.
        hkl::kappa4C::vertical::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi);

        double const & omega_c = geometry.omega()->get_consign().get_value();
        double const & chi_c = geometry.chi()->get_consign().get_value();
        double const & phi_c = geometry.phi()->get_consign().get_value();
        double komega_c, kappa_c, kphi_c;
        //this line can throw an exception so deport the source modification after.
        hkl::kappa4C::vertical::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c);

        // update the source
        _source = geometry.get_source();

        _komega->set_current(komega);
        _kappa->set_current(kappa);
        _kphi->set_current(kphi);
        _tth->set_current(geometry.tth()->get_current());

        _komega->set_consign(komega_c);
        _kappa->set_consign(kappa_c);
        _kphi->set_consign(kphi_c);
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
            if (geometry.mu()->get_current() != 0
                || geometry.gamma()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                             "\"gamma\" = \"mu\" current values must be set to zero");
              }
            else
              {
                if (geometry.mu()->get_consign() != 0
                    || geometry.gamma()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                                 "\"gamma\" = \"mu\" consign values must be set to zero");
                  }
              }
          }
        double const & omega = geometry.omega()->get_current().get_value();
        double const & chi = geometry.chi()->get_current().get_value();
        double const & phi = geometry.phi()->get_current().get_value();
        double komega, kappa, kphi;
        // the next line can throw an exception so deport the geometry modification once convertion is ok.
        hkl::kappa4C::vertical::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi);

        double const & omega_c = geometry.omega()->get_consign().get_value();
        double const & chi_c = geometry.chi()->get_consign().get_value();
        double const & phi_c = geometry.phi()->get_consign().get_value();
        double komega_c, kappa_c, kphi_c;
        // the next line can throw an exception so deport the geometry modification once convertion is ok.
        hkl::kappa4C::vertical::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c);

        // update the source
        _source = geometry.get_source();

        _komega->set_current(komega);
        _kappa->set_current(kappa);
        _kphi->set_current(kphi);
        _tth->set_current(geometry.delta()->get_current());

        _komega->set_consign(komega_c);
        _kappa->set_consign(kappa_c);
        _kphi->set_consign(kphi_c);
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
        // update the source
        if (strict)
          {
            if (geometry.mu()->get_current() != 0
                || geometry.gamma()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                             "\"gamma\" = \"mu\" current values must be set to zero");
              }
            else
              {
                if (geometry.mu()->get_consign() != 0
                    || geometry.gamma()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                                 "\"gamma\" = \"mu\" consign values must be set to zero");
                  }
              }
          }

        // update the source
        _source = geometry.get_source();

        _komega->set_current(geometry.komega()->get_current());
        _kappa->set_current(geometry.kappa()->get_current());
        _kphi->set_current(geometry.kphi()->get_current());
        _tth->set_current(geometry.delta()->get_current());

        _komega->set_consign(geometry.komega()->get_consign());
        _kappa->set_consign(geometry.kappa()->get_consign());
        _kphi->set_consign(geometry.kphi()->get_consign());
        _tth->set_consign(geometry.delta()->get_consign());
      }


    } // namespace hkl::kappa4C::vertical

  } // namespace hkl::kappa4C

} // namespace hkl
