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

#include "reflection.h"

namespace hkl
  {

  /**
   * @brief Create a Reflection.
   *
   * @param geometry The hkl::Geometry of the reflection
   * @param hkl The hkl scattering vactor.
   * @param flag if the reflection must be use during calculation.
   * @throw HKLException if the geometry is not valid.
   */

  Reflection::Reflection(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) :
      _geometry(geometry),
      _hkl(hkl),
      _flag(flag)
  {
  }

  Reflection::~Reflection()
  {
  }

  /**
   * @brief Set the hkl scattering vector store in the Reflection.
   * @param hkl The scattering vector in the crystal coordinates to store in the Reflection.
   */

  void Reflection::set_hkl(const hkl::svector & value)
  {
    _hkl = value;
  }

  /**
   * @brief Get a constant reference on the flag store in the Reflection.
   * @return The flag of the Reflection.
   *
   * the flag is true when we use the reflection in the affinement, false otherwise.
   */

  const bool & Reflection::flag() const
    {
      return _flag;
    }

  /**
   * @brief Get a constant reference on the flag store in the Reflection.
   * @return The flag of the Reflection.
   *
   * the flag is true when we use the reflection in the affinement, false otherwise.
   */

  bool & Reflection::flag()
  {
    return _flag;
  }

  /**
   * @brief compute the theoretical angle beetween two hkl vectors.
   * @param hkl The second scattering vector to compare with the Reflection internal hkl.
   * @return the angle between the two hkl.
   * @todo Maybe move this in the Sample and add a computeAngle(Reflection const & reflection)
   * @todo add the mathematical formula.
   */

  hkl::Value Reflection::computeAngle(const hkl::svector & hkl) const
    {
      return Value(_hkl.angle(hkl));
    }

  /**
   * @brief Check if two reflections are colinear.
   * @param reflection The reflection to compare with.
   * @return true if the reflections are colinear, false otherwise.
   * @todo Add the mathematical formula.
   */

  bool Reflection::isColinear(const hkl::Reflection & reflection) const
    {
      if ((_hkl.vectorialProduct(reflection._hkl)).norm2() < constant::math::epsilon)
        return true;
      else
        return false;
    }

  /**
   * \brief Are two Reflection equals ?
   * \param reflection the hkl::Reflection to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool Reflection::operator==(const hkl::Reflection & reflection) const
    {
      return _geometry == reflection._geometry
             && _hkl == reflection._hkl
             && _flag == reflection._flag
             && _hkl_phi == reflection._hkl_phi;
    }

  /**
   * @brief print the Reflection into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Reflection::printToStream(std::ostream & flux) const
    {
      flux << _hkl;


      hkl::AxeList const & axes = _geometry.get_axes();

      for (unsigned int i=0; i<axes.size(); i++)
        {
          flux.width(9);
          flux << axes[i]->get_current().get_value() * hkl::constant::math::radToDeg;
        }
      flux << " |";
      flux.width(9);
      flux << _geometry.get_source().get_waveLength().get_value();
      flux << " | " << "(" << _flag << ") hkl_phi : " << _hkl_phi;

      return flux;
    }

  /**
   * @brief print on a stream the content of the Reflection
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Reflection::toStream(std::ostream & flux) const
    {
      _geometry.toStream(flux);
      _hkl.toStream(flux);
      _hkl_phi.toStream(flux);
      flux << " " << _flag;

      return flux;
    }

  /**
   * @brief restore the content of the Reflection from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Reflection::fromStream(std::istream & flux)
  {
    _geometry.fromStream(flux);
    _hkl.fromStream(flux);
    _hkl_phi.fromStream(flux);
    flux >> _flag;

    return flux;
  }


} // namespace hkl
