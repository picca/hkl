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
#ifndef _FITPARAMETER_H
#define _FITPARAMETER_H


#include "parameter.h"
#include <string>
#include "value.h"
#include <ostream>
#include <istream>

namespace hkl
  {

  class FitParameter : public hkl::Parameter
    {
    public:
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
      FitParameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, bool toFit, double precision);


    protected:
      /**
       * @brief This flag is true if this ${class} must be fit.
       */
      bool _flagFit;

      /**
       * @brief The precision for the fit we expect on this ${class}.
       */
      hkl::Value _precision;

      /**
       * @brief The \f$\chi^2\f$ obtained after the fit.
       */
      hkl::Value _chi2;


    public:
      /**
       * @brief Get the flag of the FitParameter class.
       * @return The _flagFit bool member.
       */
      bool get_flagFit() const;

      /**
       * @brief Get the precision of the FitParameter class.
       * @return A constant Value ref on the _precision member.
       */
      const hkl::Value & get_precision() const;

      /**
       * @brief Get the _chi2 of the FitParameter class.
       * @return A constant Value ref on the _chi2 member.
       */
      const hkl::Value & get_chi2() const;

      /**
       * @brief Set the _flagFit member of the FitParameter class.
       * @param flagFit The bool to set.
       */
      void set_flagFit(bool flagFit);

      /**
       * @brief Set the _precision member of the FitParameter class.
       * @param precision The hkl::Value to set.
       */
      void set_precision(hkl::Value precision);

      /**
       * @brief Set the _chi2 member of the FitParameter class.
       * @param chi2 The hkl::Value to set.
       */
      void set_chi2(hkl::Value chi2);

      /**
       * \brief Are two FitParameter equals ?
       * \param fitParameter the FitParameter to compare with.
       * \return The comparison of the two FitParameter.
       */
      bool operator==(const FitParameter & fitParameter) const;

      /**
       * @brief shuffle the FitParameter.
       */
      void randomize();

      /*!
       * \brief print the FitParameter into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the FitParameter into a stream.
       * \param flux the stream to save the FitParameter into.
       * \return The stream with the FitParameter.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a FitParameter from a stream.
       * \param flux The stream containing the FitParameter to restore.
       * @todo call update_observers or not ?
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the FitParameter class
 * \param flux
 * \param fitParameter
 * \return The flux modified.
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::FitParameter const & fitParameter)
{
  return fitParameter.printToStream(flux);
}

#endif
