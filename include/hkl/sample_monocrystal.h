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
#ifndef _SAMPLE_MONOCRYSTAL_H
#define _SAMPLE_MONOCRYSTAL_H


#include "sample.h"
#include "svector.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class FitParameter;
}
namespace hkl
  {
  class Geometry;
}

namespace hkl
  {

  namespace sample
    {

    class MonoCrystal : public hkl::Sample
      {
      protected:
        hkl::smatrix _U;

        hkl::FitParameter * _euler_x;

        hkl::FitParameter * _euler_y;

        hkl::FitParameter * _euler_z;


      public:
        /**
         * @brief The default constructor.
         * @param geometry the geometry use to fill reflections.
         * @param name The name of the sample.
         */

        MonoCrystal(hkl::Geometry & geometry, const std::string & name);

        /**
         * @brief The default destructor.
         */

        virtual ~MonoCrystal();

        /**
         * @brief The copy constructor.
         * @param sample The sample to copy from.
         */

        MonoCrystal(const MonoCrystal & source);

        /**
         * @brief Clone the current Sample.
         * @return A pointer on the cloned sample.
         */

        virtual hkl::Sample * clone() const;

        inline const hkl::smatrix & get_U() const;

        /**
         * @brief Get the UB matrix of the Sample.
         * @return The UB matrix.
         */

        virtual hkl::smatrix get_UB();

        /**
         * @brief Get the type of the Sample.
         *
         * @return The Sample type.
         *
         * this method is use during the toStream and fromStream process.
         */

        virtual hkl::SampleType get_type();

        /**
         * @brief Compute the orientation matrix from two non colinear reflections.
         *
         * @param index1 The index of the first reflection.
         * @param index2 The index of the second reflection.
         */

        void computeU(unsigned int index1, unsigned int index2) throw(hkl::HKLException);

        bool ready_to_fit() const;

        double fitness() throw(hkl::HKLException);

        bool fitness(double & fitness);

        /**
         * @brief Randomize the crystal
         */

        void randomize();

        void update();

        /**
         * \brief Are two MonoCrystal equals ?
         * \param sample the MonoCrystal to compare with.
         * \return true if both are equals flase otherwise.
         */
        bool operator==(const MonoCrystal & sample) const;

        /**
         * @brief print on a stream the content of the MonoCrystal
         * @param flux the ostream to modify.
         * @return the modified ostream
         */
        std::ostream & toStream(std::ostream & flux) const;

        /**
         * @brief restore the content of the MonoCrystal from an istream
         * @param flux the istream.
         * @return the modified istream.
         * @todo problem of security here.
         */
        std::istream & fromStream(std::istream & flux);

      };
    inline const hkl::smatrix & MonoCrystal::get_U() const
      {
        return _U;
      }


  } // namespace hkl::sample

} // namespace hkl
#endif
