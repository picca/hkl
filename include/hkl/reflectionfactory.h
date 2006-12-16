#ifndef _REFLECTIONFACTORY_H_
#define _REFLECTIONFACTORY_H_

#include "reflection.h"

using namespace std;

namespace hkl
{

class ReflectionFactory
{
public:

    /**
     * @brief The default constructor.
     * @param geometry the Geometry use to fill the Reflection._geometry.
     * @param type the type of the Reflection.
     */
    ReflectionFactory(Geometry & geometry, ReflectionType const & type);

    virtual ~ReflectionFactory(void);

    /**
     * @brief Create a new reflection.
     * @return The created Reflection.
     */
    Reflection * create(void) throw (HKLException);

protected:

    Geometry & _geometry;

    ReflectionType _type;
};

} // namespace hkl

#endif // _REFLECTIONFACTORY_H_
