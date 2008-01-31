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

#include "fitparameter.h"

namespace hkl
  {

  /**
   * @brief Constructor
   * @param name of the FitParameter.
   * @param description The description of the FitParameter.
   * @param min the minimum of the FitParameter.
   * @param current the current value of the FitParameter.
   * @param max the maximum of the FitParameter.
   * @param toFit is a fit parameter or not
   * @param precision to fullfill for the fit.
   */
  FitParameter::FitParameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, bool toFit, double precision)  :
      Parameter(name, description, min, current, max),
      _flagFit(toFit),
      _precision(precision),
      _chi2(0)
  {
  }

  /**
   * @brief Get the flag of the FitParameter class.
   * @return The _flagFit bool member.
   */
  bool FitParameter::get_flagFit() const
    {
      return _flagFit;
    }

  /**
   * @brief Get the precision of the FitParameter class.
   * @return A constant Value ref on the _precision member.
   */
  const hkl::Value & FitParameter::get_precision() const
    {
      return _precision;
    }

  /**
   * @brief Get the _chi2 of the FitParameter class.
   * @return A constant Value ref on the _chi2 member.
   */
  const hkl::Value & FitParameter::get_chi2() const
    {
      return _chi2;
    }

  /**
   * @brief Set the _flagFit member of the FitParameter class.
   * @param flagFit The bool to set.
   */
  void FitParameter::set_flagFit(bool flagFit)
  {
    _flagFit = flagFit;
  }

  /**
   * @brief Set the _precision member of the FitParameter class.
   * @param precision The hkl::Value to set.
   */
  void FitParameter::set_precision(hkl::Value precision)
  {
    _precision = precision;
  }

  /**
   * @brief Set the _chi2 member of the FitParameter class.
   * @param chi2 The hkl::Value to set.
   */
  void FitParameter::set_chi2(hkl::Value chi2)
  {
    _chi2 = chi2;
  }

  /**
   * \brief Are two FitParameter equals ?
   * \param fitParameter the FitParameter to compare with.
   * \return The comparison of the two FitParameter.
   */
  bool FitParameter::operator==(const hkl::FitParameter & fitParameter) const
    {
      return Parameter::operator==(fitParameter)
             && _flagFit == fitParameter._flagFit
             && _precision == fitParameter._precision
             && _chi2 == fitParameter._chi2;
    }

  /**
   * @brief shuffle the FitParameter.
   */
  void FitParameter::randomize()
  {
    if (_flagFit)
      {
        Value d = get_min() + (get_max()-get_min()) * ((double)rand()/(RAND_MAX+1.0));
        set_current(d);
      }
  }

  /*!
   * \brief print the FitParameter into a flux
   * \param flux The stream to print into.
   */
  std::ostream & FitParameter::printToStream(std::ostream & flux) const
    {
      flux << std::showpoint;
      Parameter::printToStream(flux);
      flux  << " Ŧ " << _precision.get_value() << ", "
      << "chi2: " << _chi2.get_value() << ", "
      << "To fit: " << _flagFit;
      flux << std::noshowpoint << std::noshowpos << std::dec;

      return flux;
    }

  /*!
   * \brief Save the FitParameter into a stream.
   * \param flux the stream to save the FitParameter into.
   * \return The stream with the FitParameter.
   */
  std::ostream & FitParameter::toStream(std::ostream & flux) const
    {
      Parameter::toStream(flux);
      _precision.toStream(flux);
      _chi2.toStream(flux);
      flux << " " << _flagFit << std::endl;

      return flux;
    }

  /*!
   * \brief Restore a FitParameter from a stream.
   * \param flux The stream containing the FitParameter to restore.
   * @todo call update_observers or not ?
   */
  std::istream & FitParameter::fromStream(std::istream & flux)
  {
    Parameter::fromStream(flux);
    _precision.fromStream(flux);
    _chi2.fromStream(flux);
    flux >> _flagFit;

    return flux;
  }


} // namespace hkl
