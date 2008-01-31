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
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      /**
       *  @brief Default constructor
       */
      Geometry::Geometry() :
          hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        // sample holder
        hkl::Holder * sample = _holders.add();
        _omega = sample->add_rotation("omega", svector(0., -1., 0.));

        //detector holder
        hkl::Holder * detector = _holders.add();
        _tth = detector->add_rotation("tth", svector(0., -1., 0.));
      }

      /**
       * @brief Another constructor.
       * @param omega the first angle value.
       * @param tth the second angle value.
       */
      Geometry::Geometry(double omega, double tth) :
          hkl::Geometry("2 circles", "The Cristal beamline (synchrotron-soleil) france diffractometer.")
      {
        _source.setDirection(svector(1,0,0));

        // sample holder
        hkl::Holder * sample = _holders.add();
        _omega = sample->add_rotation("omega", svector(0., -1., 0.));

        //detector holder
        hkl::Holder * detector = _holders.add();
        _tth = detector->add_rotation("tth", svector(0., -1., 0.));

        this->set_angles(omega, tth);
        this->set_angles_consign(omega, tth);
      }

      Geometry::~Geometry()
      {
      }

      /**
       * @brief Copy Constructor.
       */
      Geometry::Geometry(const hkl::twoC::vertical::Geometry & geometry) :
          hkl::Geometry(geometry)
      {
        _omega = static_cast<hkl::axe::Rotation *>(_holders.axes()["omega"]);
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
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles(double omega, double tth)
      {
        _omega->set_current(omega);
        _tth->set_current(tth);
      }

      /**
       * @brief Set the angles of the eulerian4CD::Vertical geometry.
       * @param omega The value of the "omega" Axe.
       * @param tth The value of the "tth" Axe.
       */
      void Geometry::set_angles_consign(double omega, double tth)
      {
        _omega->set_consign(omega);
        _tth->set_consign(tth);
      }

      /**
       * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
       * @param geometry The hkl::eulerian4C::vertical::Geometry.
       * @param strict false or true if we must not care of the strictness of the conversion.
       * @throw HKLException
       */
      void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException)
      {
        // check that chi and phi current and consign values are compatible with the convertion in case of a strict conversion
        if (strict)
          {
            // first the current value
            if (geometry.chi()->get_current() != 0 || geometry.phi()->get_current() != 0)
              {
                HKLEXCEPTION("\"chi\" and/or \"phi\" current values are wrong",
                             "\"chi\" = \"phi\" current values must be set to zero");
              }
            else
              {
                // the the consign values
                if (geometry.chi()->get_consign() != 0 || geometry.phi()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"chi\" and/or \"phi\" consign values are wrong",
                                 "\"chi\" = \"phi\" consign values must be set to zero");
                  }
              }
          }
        // everything ok so we can set the Geometry.
        _source = geometry.get_source();

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
        // check that kappa and kphi current and consign values are compatible with the convertion in case of a strict conversion
        if (strict)
          {
            // first the current value
            if (geometry.kappa()->get_current() != 0 || geometry.kphi()->get_current() != 0)
              {
                HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) current values are wrong",
                             "\"kappa\" = \"kphi\" current values must be set to zero");
              }
            else
              {
                // the the consign values
                if (geometry.kappa()->get_consign() != 0 || geometry.kphi()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"kappa\" and/or \"kphi\" axe(s) consign values are wrong",
                                 "\"kappa\" = \"kphi\" consign values must be set to zero");
                  }
              }
          }
        // everything ok so we can set the Geometry.
        _source = geometry.get_source();

        _omega->set_current(geometry.komega()->get_current());
        _tth->set_current(geometry.tth()->get_current());

        _omega->set_consign(geometry.komega()->get_consign());
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
        // check that gamma, mu, chi and phi current and consign values are compatible with the convertion
        if (strict)
          {
            // first the current value
            if (geometry.gamma()->get_current() != 0
                || geometry.mu()->get_current() != 0
                || geometry.chi()->get_current() != 0
                || geometry.phi()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" current values are wrong",
                             "\"gamma\" = \"mu\" = \"chi\" = \"phi\" current values must be set to zero");
              }
            else
              {
                // the the consign values
                if (geometry.gamma()->get_consign() != 0
                    || geometry.mu()->get_consign() != 0
                    || geometry.chi()->get_consign() != 0
                    || geometry.phi()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"chi\" and/or \"phi\" consign values are wrong",
                                 "\"gamma\" = \"mu\" = \"chi\" = \"phi\" consign values must be set to zero");
                  }
              }
          }
        // ok so set the Geometry
        _source = geometry.get_source();

        _omega->set_current(geometry.omega()->get_current());
        _tth->set_current(geometry.delta()->get_current());

        _omega->set_consign(geometry.omega()->get_consign());
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
        // check that gamma, mu, kappa and kphi current and consign values are compatible with the convertion
        if (strict)
          {
            // first the current value
            if (geometry.gamma()->get_current() != 0
                || geometry.mu()->get_current() != 0
                || geometry.kappa()->get_current() != 0
                || geometry.kphi()->get_current() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" current values are wrong",
                             "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" current values must be set to zero");
              }
            else
              {
                // the the consign values
                if (geometry.gamma()->get_consign() != 0
                    || geometry.mu()->get_consign() != 0
                    || geometry.kappa()->get_consign() != 0
                    || geometry.kphi()->get_consign() != 0)
                  {
                    HKLEXCEPTION("\"gamma\" and/or \"mu\" and/or \"kappa\" and/or \"kphi\" consign values are wrong",
                                 "\"gamma\" = \"mu\" = \"kappa\" = \"kphi\" consign values must be set to zero");
                  }
              }
          }
        _source = geometry.get_source();

        _omega->set_current(geometry.komega()->get_current());
        _tth->set_current(geometry.delta()->get_current());

        _omega->set_consign(geometry.komega()->get_consign());
        _tth->set_consign(geometry.delta()->get_consign());
      }


    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
