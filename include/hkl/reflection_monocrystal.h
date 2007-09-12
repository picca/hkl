#ifndef _REFLECTION_MONOCRYSTAL_H
#define _REFLECTION_MONOCRYSTAL_H


#include "reflection.h"
#include "HKLException.h"

namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class svector;
}

namespace hkl
  {

  namespace reflection
    {

    class MonoCrystal : public hkl::Reflection
      {
      public:
        /**
         * @brief The default constructor.
         *
         * @param geometry The geometry use to initialize the geometry store in the reflections.
         * @param hkl the scattering vector of the reflection.
         * @param flag the falg of the reflection (related to the affinement).
         * @throw HKLException if the geometry is not valid.
         */

        MonoCrystal(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) throw(hkl::HKLException);

        virtual ~MonoCrystal();

        /**
         * @brief The copy constructor
         * @param reflection  The Reflection to copy.
         */

        MonoCrystal(const MonoCrystal & source);

        /**
         * @brief clone the reflection.
         * @return a cloned reflection.
         */

        hkl::Reflection * clone() const;

      };

  } // namespace hkl::reflection

} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class reflection
 * @param flux The flux to print into
 * @param reflection The Reflection to print.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::reflection::MonoCrystal const & reflection)
{
  return reflection.printToStream(flux);
}
#endif
