#ifndef _REFLECTIONFACTORY_MONOCRYSTAL_H_
#define _REFLECTIONFACTORY_MONOCRYSTAL_H_

#include "reflectionfactory.h"

using namespace std;

namespace hkl {
    namespace reflectionfactory {

        class MonoCrystal : public ReflectionFactory
          {
          public:

            /** 
             * @brief The default constructor
             * 
             * @param geometry the Geometry use to fill the Reflection._geometry.
             */
            MonoCrystal(Geometry & geometry);

            virtual ~MonoCrystal(void);

            /** 
             * @brief Create a new reflection.
             * 
             * @return The created Reflection.
             */
            Reflection * create(void);
          };

    } // namespace reflectionfactory
} // namespace hkl

#endif // _REFLECTIONFACTORY_MONOCRYSTAL_H_
