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

#include "diffractometer.h"

#include "portability.h"
namespace hkl
  {

  Diffractometer::Diffractometer(const std::string & name, const std::string & description) :
      HKLObject(name, description)
  {
  }

  Diffractometer::~Diffractometer()
  {
  }

  /**
   * \brief Are two Diffractometer equals ?
   * \param diffractometer the hkl::Diffractometer to compare with.
   * \return true if both are equals false otherwise.
   */
  bool Diffractometer::operator==(const hkl::Diffractometer & diffractometer) const
    {
      return HKLObject::operator==(diffractometer)
             && *_geometry == *(diffractometer._geometry)
             && *_samples == *(diffractometer._samples)
             && _modes == diffractometer._modes
             && _pseudoAxeEngines == diffractometer._pseudoAxeEngines;
    }

  /**
   * @brief print the Diffractometer into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Diffractometer::printToStream(std::ostream & flux) const
    {
      flux << std::endl;
      flux << "Diffractometer: \"" << get_name() << "\"" << std::endl;
      HKLObject::printToStream(flux);
      _geometry->printToStream(flux);
      _samples->printToStream(flux);
      _modes.printToStream(flux);
      _pseudoAxeEngines.printToStream(flux);

      return flux;
    }

  /**
   * @brief print on a stream the content of the Diffractometer
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Diffractometer::toStream(std::ostream & flux) const
    {
      flux << " " << HKL_VERSION;
      HKLObject::toStream(flux);
      _geometry->toStream(flux);
      _samples->toStream(flux);
      _modes.toStream(flux);
      _pseudoAxeEngines.toStream(flux);

      return flux;
    }

  /**
   * @brief restore the content of the Diffractometer from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Diffractometer::fromStream(std::istream & flux)
  {
    unsigned int version;

    flux >> version;
    if (version == HKL_VERSION)
      {
        HKLObject::fromStream(flux);
        _geometry->fromStream(flux);
        _samples->fromStream(flux);
        _modes.fromStream(flux);
        _pseudoAxeEngines.fromStream(flux);
      }
    return flux;
  }


} // namespace hkl
