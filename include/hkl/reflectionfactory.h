#ifndef _REFLECTIONFACTORY_H_
#define _REFLECTIONFACTORY_H_

#include "reflection.h"

using namespace std;

namespace hkl {

    class ReflectionFactory
      {
      public:

        virtual ~ReflectionFactory(void);

        /** 
         * @brief Create a new reflection.
         * 
         * @return The created Reflection.
         */
        virtual Reflection * create(void) = 0;

      protected:

        /** 
         * @brief The default constructor
         * 
         * @param geometry the Geometry use to fill the Reflection._geometry.
         */
        ReflectionFactory(Geometry & geometry);

        Geometry & _geometry;

      };

} // namespace hkl

#endif // _REFLECTIONFACTORY_H_
