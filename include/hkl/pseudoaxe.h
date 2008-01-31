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
#ifndef _PSEUDOAXE_H
#define _PSEUDOAXE_H


#include "object.h"
#include "value.h"
#include <string>
#include "HKLException.h"
#include <ostream>

namespace hkl
  {
  class PseudoAxeEngine;
}
namespace hkl
  {
  class AxeList;
}
namespace hkl
  {
  class ParameterList;
}

namespace hkl
  {

  class PseudoAxe : public hkl::ObjectReadOnly
    {
      friend class hkl::PseudoAxeEngine;

    protected:
      hkl::Value _min;

      hkl::Value _current;

      hkl::Value _consign;

      hkl::Value _max;

      hkl::PseudoAxeEngine * _engine;


    public:
      /**
       * @brief The default constructor.
       * @param name The name of the PseudoAxeTemp.
       * @param description The description of the PseudoAxeTemp.
       * @param engine The engine use to compute the pseudoAxes value.
       * @todo be sure to be consistant with ModeTemp.
       */
      PseudoAxe(const std::string & name, const std::string & description, hkl::PseudoAxeEngine * engine);

      AxeList & relatedAxes();

      /**
       * @brief Initialize the pseudoAxe.
       * This method must be call before using a pseudoAxe.
       */
      void initialize() throw(hkl::HKLException);

      /**
       * @brief uninitialize the PseudoAxe.
       * Uninitialize a PseudoAxe if you do not whant to use it.
       */
      void uninitialize();

      /**
       * @brief Get the initialized state of the PseudoAxe.
       * @return A bool fill with the initialized state of the PseudoAxe.
       */
      bool is_initialized() const;

      /**
       * @brief Get the readabled state of the PseudoAxe.
       * @return A bool fill with the readable state of the PseudoAxe.
       */
      bool is_readable() const;

      /**
       * @brief Get the writable state of the PseudoAxe.
       * @return A bool fill with the writable state of the PseudoAxe.
       */
      bool is_writable() const;

      /**
       * @brief Get the min Value of the PseudoAxe.
       * @return A Value fill with the minimum value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      const hkl::Value & get_min() const throw(hkl::HKLException);

      /**
       * @brief Get the current Value of the PseudoAxe.
       * @return A Value fill with the current value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      const hkl::Value & get_current() const throw(hkl::HKLException);

      /**
       * @brief Get the current Value of the PseudoAxe.
       * @return A Value fill with the current write value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      hkl::Value const & get_consign() const throw(hkl::HKLException);

      /**
       * @brief Get the maximum Value of the PseudoAxe.
       * @return A Value fill with the maximum Value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      const hkl::Value & get_max() const throw(hkl::HKLException);

      /**
       * @brief Set the consign value of the PseudoAxe.
       * @param value The Value to set.
       * @throw HKLException If the PseudoAxe is not writable.
       *
       * This method set the write part of the pseudoAxe and compute
       * the corresponding geometry using the engine.
       */
      void set_consign(const hkl::Value & value) throw(hkl::HKLException);

      /**
       * @brief Set the engine use by the PseudoAxe.
       * @param engine The engine to set.
       *
       * This method is only use by the DerivedPseudoAxeEngine to modify
       * the engine part of the PseudoAxe.
       */
      void set_engine(hkl::PseudoAxeEngine * engine);

      hkl::ParameterList & parameters();

      /**
       * \brief Are two PseudoAxe equals ?
       * \param pseudoAxe the PseudoAxe to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const PseudoAxe & pseudoAxe) const;

      /**
       * @brief print the PseudoAxe into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };

} // namespace hkl
/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}
#endif
