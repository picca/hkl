#ifndef _REFLECTION_MONOCRYSTAL_H_
#define _REFLECTION_MONOCRYSTAL_H_

#include "reflection.h"

using namespace std;

namespace hkl
  {
  namespace reflection
    {

    class MonoCrystal : public Reflection
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
        MonoCrystal(Geometry const & geometry,
                    svector const & hkl,
                    bool const & flag) throw (HKLException);

        /**
         * @brief The copy constructor
         * @param reflection  The Reflection to copy.
         */
        MonoCrystal(MonoCrystal const & reflection);

        /**
         * @brief the default destructor.
         */
        virtual ~MonoCrystal(void);

        /**
         * @brief clone the reflection.
         * @return a cloned reflection.
         */
        Reflection * clone(void) const;
      };

  } // namespace reflection
} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class reflection
 * @param flux The flux to print into
 * @param reflection The Reflection to print.
 */
static ostream &
operator << (ostream & flux, hkl::reflection::MonoCrystal const & reflection)
{
  return reflection.printToStream(flux);
}

#endif // _REFLECTION_MONOCRYSTAL_H_
