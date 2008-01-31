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
#ifndef _AFFINEMENT_H
#define _AFFINEMENT_H


#include "object.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class FitParameterList;
}

namespace hkl
  {

  class Affinement : public hkl::Object
    {
    protected:
      unsigned int _nb_max_iterations;

      unsigned int _nb_iterations;

      double _fitness;


    public:
      /**
       * @brief the default constructor protected because the class is abstrait
       * @param name The name of the Affinement.
       * @param description The description of the Affinement.
       */

      Affinement(const std::string & name, const std::string & description);

      virtual ~Affinement();

      /**
       * @brief fit the parameter of an objects
       * @param fitParameterList The hkl::FitParameterList to fit.
       *
       * this function modify the object.
       */

      virtual void fit(hkl::FitParameterList & fitParameterList) throw(hkl::HKLException) = 0;

      inline const unsigned int get_nb_max_iterations() const;

      void set_nb_max_iterations(const unsigned int value);

      inline const unsigned int get_nb_iterations() const;

      void set_nb_iterations(const unsigned int value);

      inline const double get_fitness() const;

      void set_fitness(const double value);

      /**
       * @brief print the Affinement into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Affinement
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Affinement from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };
  inline const unsigned int Affinement::get_nb_max_iterations() const
    {
      return _nb_max_iterations;
    }

  inline const unsigned int Affinement::get_nb_iterations() const
    {
      return _nb_iterations;
    }

  inline const double Affinement::get_fitness() const
    {
      return _fitness;
    }


} // namespace hkl

/**
 * \brief Overload of the << operator for the Affinement class
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::Affinement const & affinement)
{
  return affinement.printToStream(flux);
}
#endif
