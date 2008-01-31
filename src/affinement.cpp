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

#include "affinement.h"
#include "fitparameterlist.h"

#include <iomanip>
#include "constant.h"
namespace hkl
  {

  /**
   * @brief the default constructor protected because the class is abstrait
   * @param name The name of the Affinement.
   * @param description The description of the Affinement.
   */

  Affinement::Affinement(const std::string & name, const std::string & description) :
      Object(name, description),
      _nb_max_iterations(2500),
      _nb_iterations(0),
      _fitness(0)
  {
  }

  Affinement::~Affinement()
  {
  }

  void Affinement::set_nb_max_iterations(const unsigned int value)
  {
    _nb_max_iterations = value;
  }

  void Affinement::set_nb_iterations(const unsigned int value)
  {
    _nb_iterations = value;
  }

  void Affinement::set_fitness(const double value)
  {
    _fitness = value;
  }

  /**
   * @brief print the Affinement into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Affinement::printToStream(std::ostream & flux) const
    {
      flux << "Affinement : " << Object::get_name() << std::endl;
      flux << Object::get_description() << std::endl;
      flux << "  max iterations : " << _nb_max_iterations << std::endl;
      flux << "      iterations : " << _nb_iterations << std::endl;
      flux << "         fitness : " << _fitness << std::endl;

      return flux;
    }

  /**
   * @brief print on a stream the content of the Affinement
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Affinement::toStream(std::ostream & flux) const
    {
      Object::toStream(flux);
      flux << " " << _nb_max_iterations
      << " " << _nb_iterations
      << std::setprecision(constant::math::precision)
      << " " << _fitness << std::endl;

      return flux;
    }

  /**
   * @brief restore the content of the Affinement from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Affinement::fromStream(std::istream & flux)
  {
    Object::fromStream(flux);
    flux >> _nb_max_iterations
    >> _nb_iterations
    >> std::setprecision(constant::math::precision)
    >> _fitness;

    return flux;
  }


} // namespace hkl
