#ifndef _GEOMETRY_KAPPA_H
#define _GEOMETRY_KAPPA_H


#include "geometry.h"
#include <string>
#include <ostream>
#include <istream>

namespace hkl
  {

  namespace geometry
    {

    class Kappa : public hkl::Geometry
      {
      protected:
        double _alpha;

        Kappa(const std::string & name, const std::string & description, double alpha);


      public:
        virtual ~Kappa();

        inline const double get_alpha() const;

        /**
         * @brief print the Kappa into a flux
         * @param flux The stream to print into.
         * @return The modified flux.
         */
        std::ostream & printToStream(std::ostream & flux) const;

      };

    inline const double Kappa::get_alpha() const
      {
        return _alpha;
      }


  } // namespace hkl::geometry

} // namespace hkl
#endif
