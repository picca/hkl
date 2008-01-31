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
#ifndef _DERIVED_MODE_H
#define _DERIVED_MODE_H


#include "mode.h"
#include <string>
#include "value.h"
#include "svector.h"

namespace hkl
  {

  namespace mode
    {

    template<class T, class C>
    class Derived : public hkl::ModeTemp<T>
      {
      private:
        mutable typename C::value_type _gconv;

        mutable C * _mode;


      public:
        /**
          * @brief The default constructor.
          * @param name The name of the Derived.
          * @param description The description of the Derived.
          * @param geometry The Geometry for the computation.
          */

        Derived(const std::string & name, const std::string & description, T & geometry);

        virtual ~Derived();

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first coordinate.
         * @param k The scaterring vector second coordinate.
         * @param l The scaterring vector third coordinate.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         */

        virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

      };
    /**
      * @brief The default constructor.
      * @param name The name of the Derived<T, C>.
      * @param description The description of the Derived<T, C>.
      * @param geometry The Geometry for the computation.
      */

    template<class T, class C>
    Derived<T, C>::Derived(const std::string & name, const std::string & description, T & geometry) :
        ModeTemp<T>(name, description, geometry)
    {
      // Bouml preserved body begin 00035E82
      _mode = new C("derived", "real mode", _gconv);
      ModeTemp<T>::_parameters = _mode->parameters();
      // Bouml preserved body end 00035E82
    }

    template<class T, class C>
    Derived<T, C>::~Derived()
    {
      // Bouml preserved body begin 00035F02
      delete _mode;
      // Bouml preserved body end 00035F02
    }

    /**
     * @brief The main function to get a sample of angles from (h,k,l).
     * @param h The scaterring vector first coordinate.
     * @param k The scaterring vector second coordinate.
     * @param l The scaterring vector third coordinate.
     * @param UB The product of the orientation matrix U by the crystal matrix B.
     */

    template<class T, class C>
    void Derived<T, C>::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const
      {
        // Bouml preserved body begin 00035F82
        _gconv.setFromGeometry(ModeTemp<T>::_geometry, false);
        _mode->computeAngles(h, k, l, UB);
        ModeTemp<T>::_geometry.setFromGeometry(_gconv, true);
        // Bouml preserved body end 00035F82
      }


  } // namespace hkl::mode

} // namespace hkl
#endif
