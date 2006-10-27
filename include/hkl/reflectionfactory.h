#ifndef _REFLECTIONFACTORY_H_
#define _REFLECTIONFACTORY_H_

#include "reflection.h"
#include "enums.h"

using namespace std;

namespace hkl {

    class ReflectionFactory
      {
      public:

        /** 
         * @brief The default constructor
         * 
         * @param geometry the Geometry use to fill the Reflection._geometry.
         */
        ReflectionFactory(Geometry & geometry, ReflectionType const & type);

        virtual ~ReflectionFactory(void);

        /** 
         * @brief Create a new reflection.
         * 
         * @return The created Reflection.
         */
        Reflection * create(void);

      protected:

        Geometry & _geometry;

        ReflectionType _type;
      };

} // namespace hkl

#endif // _REFLECTIONFACTORY_H_
