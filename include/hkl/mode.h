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
#ifndef _MODE_H
#define _MODE_H


#include "hklobject.h"
#include <string>
#include "value.h"
#include "svector.h"
#include "HKLException.h"

#include "convenience.h"
namespace hkl
  {

  class Mode : public hkl::HKLObject
    {
    protected:
      /**
       * @brief The default constructor of the Mode class.
       * @param name the name of the Mode.
       * @param description the description of the Mode.
       */

      Mode(const std::string & name, const std::string & description);


    public:
      virtual ~Mode();

      /**
       * @brief The main function to get a sample of angles from (h,k,l).
       * @param h The scaterring vector first coordinate.
       * @param k The scaterring vector second coordinate.
       * @param l The scaterring vector third coordinate.
       * @param UB The product of the orientation matrix U by the crystal matrix B.
       */

      virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const = 0;

    };
  template<class T>
  class ModeTemp : public hkl::Mode
    {
    protected:
      /**
       * @brief Default constructor - protected to make sure this class is abstract.
       * @param name The name of the ModeTemp.
       * @param description The description of the ModeTemp.
       * @param geometry the Geometry use to do calculation.
       */

      ModeTemp(const std::string & name, const std::string & description, T & geometry);


    public:
      virtual ~ModeTemp();


    protected:
      T & _geometry;


    public:
      typedef T value_type;


    protected:
      /**
       * @brief Check if the parameter are ok to compute the geometry configuration.
       *
       * @param h
       * @param k
       * @param l
       * @param UB
       * @throw HKLException if one of the parameters is wrong.
       * @return true if parameters are ok, false otherwise.
       */

      bool _parametersAreOk(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const throw(hkl::HKLException);

      /**
       * @brief Compute theta correspondig to thoses parameters.
       *
       * @param h The first coordinate of the hkl vector.
       * @param k the second coordinate of the hkl vector.
       * @param l the third coordinate of the hkl vector.
       * @param UB The UB matrix.
       * @param[out] theta the scattering reflection angle.
       * @param[out] hphi the hkl vector in the sample holder coordinates.
       * @throw HKLException if the reflection is unreachable
       */

      void _computeThetaAndHphi(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB, double & theta, hkl::svector & hphi) const throw(hkl::HKLException);

    };
  /**
   * @brief Default constructor - protected to make sure this class is abstract.
   * @param name The name of the ModeTemp.
   * @param description The description of the ModeTemp.
   * @param geometry the Geometry use to do calculation.
   */

  template<class T>
  ModeTemp<T>::ModeTemp(const std::string & name, const std::string & description, T & geometry) :
      Mode(name, description),
      _geometry(geometry)
  {
    // Bouml preserved body begin 00034702
    // Bouml preserved body end 00034702
  }

  template<class T>
  ModeTemp<T>::~ModeTemp()
  {
    // Bouml preserved body begin 00034782
    // Bouml preserved body end 00034782
  }

  /**
   * @brief Check if the parameter are ok to compute the geometry configuration.
   *
   * @param h
   * @param k
   * @param l
   * @param UB
   * @throw HKLException if one of the parameters is wrong.
   * @return true if parameters are ok, false otherwise.
   */

  template<class T>
  bool ModeTemp<T>::_parametersAreOk(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00034802
    // Check [h,k,l]
    if (fabs(h.get_value()) < constant::math::epsilon
        && fabs(k.get_value()) < constant::math::epsilon
        && fabs(l.get_value()) < constant::math::epsilon)
      HKLEXCEPTION("Cannot compute the geometry axes values of the [0,0,0] reflection.",
                   "Please set an non-null [h,k,l]");

    // check the wave length
    if (_geometry.get_source().get_waveLength().get_value() < constant::math::epsilon)
      HKLEXCEPTION("Cannot compute the geometry axes values with a null wave length.",
                   "Please set an non-null wavelength.");

    if (UB == smatrix())
      HKLEXCEPTION("Cannot compute the geometry axes values with a null UB matrix",
                   "please set a correct UB matrix.");

    return true;
    // Bouml preserved body end 00034802
  }

  /**
   * @brief Compute theta correspondig to thoses parameters.
   *
   * @param h The first coordinate of the hkl vector.
   * @param k the second coordinate of the hkl vector.
   * @param l the third coordinate of the hkl vector.
   * @param UB The UB matrix.
   * @param[out] theta the scattering reflection angle.
   * @param[out] hphi the hkl vector in the sample holder coordinates.
   * @throw HKLException if the reflection is unreachable
   */

  template<class T>
  void ModeTemp<T>::_computeThetaAndHphi(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB, double & theta, hkl::svector & hphi) const throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00034882
    // Calcule de Theta
    hphi = UB * svector(h.get_value(),k.get_value(),l.get_value());
    try
      {
        double lambda = _geometry.get_source().get_waveLength().get_value();
        theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
      }
    catch (const HKLException &)
      {
        HKLEXCEPTION("Unreachable reflection with this energy.",
                     "Please change h k l values or the energy.");
      }
    // Bouml preserved body end 00034882
  }


} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << for the Mode class
 * \param flux The flux to modifie
 * \param mode The mode to stream.
 * \return The modified flux.
 */
template<typename T>
std::ostream &
operator << (std::ostream & flux, hkl::ModeTemp<T> const & mode)
{
  return mode.printToStream(flux);
};

#endif
