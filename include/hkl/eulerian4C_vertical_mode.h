#ifndef _EULERIAN4C_VERTICAL_MODE_H
#define _EULERIAN4C_VERTICAL_MODE_H


#include "mode.h"
#include <string>
#include "eulerian4C_vertical_geometry.h"

namespace hkl
  {
  class Value;
}
namespace hkl
  {
  class smatrix;
}
namespace hkl
  {
  class Parameter;
}

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      namespace mode
        {

        class Bissector : public hkl::ModeTemp<hkl::eulerian4C::vertical::Geometry>
          {
          public:
            Bissector(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry);

            virtual ~Bissector();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };
        class Delta_Theta : public hkl::ModeTemp<hkl::eulerian4C::vertical::Geometry>
          {
          protected:
            hkl::Parameter * _dtheta;


          public:
            Delta_Theta(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry);

            virtual ~Delta_Theta();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };
        class Constant_Omega : public hkl::ModeTemp<hkl::eulerian4C::vertical::Geometry>
          {
          protected:
            hkl::Parameter * _omega;


          public:
            Constant_Omega(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry);

            virtual ~Constant_Omega();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };
        class Constant_Chi : public hkl::ModeTemp<hkl::eulerian4C::vertical::Geometry>
          {
          protected:
            hkl::Parameter * _chi;


          public:
            Constant_Chi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry);

            virtual ~Constant_Chi();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };
        class Constant_Phi : public hkl::ModeTemp<hkl::eulerian4C::vertical::Geometry>
          {
          protected:
            hkl::Parameter * _phi;


          public:
            Constant_Phi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry);

            virtual ~Constant_Phi();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };

      } // namespace hkl::eulerian4C::vertical::mode

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
#endif
