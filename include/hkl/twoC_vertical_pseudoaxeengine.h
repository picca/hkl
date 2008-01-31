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
#ifndef _TWOC_VERTICAL_PSEUDOAXEENGINE_H
#define _TWOC_VERTICAL_PSEUDOAXEENGINE_H


#include "pseudoaxeengine.h"
#include "twoC_vertical_geometry.h"
#include "HKLException.h"
#include <ostream>
#include <istream>
#include "interval.h"

namespace hkl
  {
  namespace axe
    {
    class Rotation;
  }
}
namespace hkl
  {
  class PseudoAxe;
}

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      namespace pseudoAxeEngine
        {

        class Th2th : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
          {
          protected:
            hkl::axe::Rotation * _omega;

            double _omega0;

            hkl::axe::Rotation * _tth;

            double _tth0;

            hkl::PseudoAxe * _th2th;


          public:
            Th2th(hkl::twoC::vertical::Geometry & geometry);

            ~Th2th();

            /**
             * @brief Initialize the pseudoAxe.
             *
             * This method must be call before using a pseudoAxe.
             */
            virtual void initialize() throw(hkl::HKLException);

            virtual void update();

            /**
             * @brief set the current value of the PseudoAxe.
             * @throw HKLException if the pseudoAxe is not ready to be set.
             */
            virtual void set() throw(hkl::HKLException);

            /**
             * @brief print on a stream the content of the Th2th
             * @param flux the ostream to modify.
             * @return the modified ostream
             */
            std::ostream & toStream(std::ostream & flux) const;

            /**
             * @brief restore the content of the Th2th from an istream
             * @param flux the istream.
             * @return the modified istream.
             * @todo problem of security here.
             */
            std::istream & fromStream(std::istream & flux);

          };
        class Q2th : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
          {
          protected:
            hkl::axe::Rotation * _omega;

            double _omega0;

            hkl::axe::Rotation * _tth;

            double _tth0;

            hkl::PseudoAxe * _q2th;


          public:
            Q2th(hkl::twoC::vertical::Geometry & geometry);

            ~Q2th();

            /**
             * @brief Initialize the pseudoAxe.
             *
             * This method must be call before using a pseudoAxe.
             */
            virtual void initialize() throw(hkl::HKLException);

            virtual void update();

            /**
             * @brief set the current value of the PseudoAxe.
             * @throw HKLException if the pseudoAxe is not ready to be set.
             */
            virtual void set() throw(hkl::HKLException);

            /**
             * @brief print on a stream the content of the Q2th
             * @param flux the ostream to modify.
             * @return the modified ostream
             */
            std::ostream & toStream(std::ostream & flux) const;

            /**
             * @brief restore the content of the Q2th from an istream
             * @param flux the istream.
             * @return the modified istream.
             * @todo problem of security here.
             */
            std::istream & fromStream(std::istream & flux);

          };
        class Q : public hkl::PseudoAxeEngineTemp<hkl::twoC::vertical::Geometry>
          {
          protected:
            hkl::axe::Rotation * _tth;

            hkl::PseudoAxe * _q;


          public:
            Q(hkl::twoC::vertical::Geometry & geometry);

            ~Q();

            /**
             * @brief Initialize the pseudoAxe.
             *
             * This method must be call before using a pseudoAxe.
             */
            virtual void initialize() throw(hkl::HKLException);

            virtual void update();

            /**
             * @brief set the current value of the PseudoAxe.
             * @throw HKLException if the pseudoAxe is not ready to be set.
             */
            virtual void set() throw(hkl::HKLException);

            /**
             * @brief Un-Initialize the pseudoAxe.
             * This method must be call to un-initialize a pseudoAxe.
             */
            virtual void uninitialize();

            /**
             * @brief print on a stream the content of the Q
             * @param flux the ostream to modify.
             * @return the modified ostream
             */
            std::ostream & toStream(std::ostream & flux) const;

            /**
             * @brief restore the content of the Q from an istream
             * @param flux the istream.
             * @return the modified istream.
             * @todo problem of security here.
             */
            std::istream & fromStream(std::istream & flux);

          };

      } // namespace hkl::twoC::vertical::pseudoAxeEngine

    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
#endif
