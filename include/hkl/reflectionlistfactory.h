#ifndef _REFLECTIONLISTFACTORY_H_
#define _REFLECTIONLISTFACTORY_H_

#include "reflectionfactory.h"

using namespace std;

namespace hkl {

    class ReflectionListFactory
      {
      public:

        virtual ~ReflectionListFactory(void);

        /** 
         * @brief Make a deep copy of a ReflectionListFactory.
         * 
         * @return A pointer on the copied ReflectionListFactory.
         */
        virtual ReflectionListFactory * clone(void) const = 0;

        /** 
         * @brief Add a reflection to the ReflectionList.
         * 
         * @param reflection The reflection to add.
         * 
         * @return The index of the added reflection.
         */
        Reflection & add(Value const & h, Value const & k, Value const & l);

        /** 
         * @brief Delete the ith reflection
         * 
         * @param index of the reflection to delete.
         * @throw HKLException if index is out of range.
         */
        void del(unsigned int index) throw (HKLException);

        /** 
         * @brief Return the number of reflection in the ReflectionList.
         * @return The number of reflection in the ReflectionList.
         */
        unsigned int size(void) const;

        /** 
         * @brief Return the number of undependant Reflection in the ReflectionList.
         * 
         * @return The number of non-colinear Reflection in the ReflectionList.
         */
        unsigned int size_indep(void) const;

        /** 
         * @brief Return a reference on the ReflectionList ith Reflection.
         * 
         * @param index of the returned Reflection. 
         * @throw HKLException if index is out of range. 
         * 
         * @return The ith Reflection.
         */
        Reflection & operator[](unsigned int index) throw (HKLException);

        bool operator==(ReflectionListFactory const & reflectionListFactory) const;

        ostream & printToStream(ostream & flux) const;

        /** 
         * @brief Serialize the ReflectionListFactory.
         * 
         * @param  flux The stream to save the ReflectionListFactory into.
         * 
         * @return the flux with the ReflectionListFactory serialized. 
         */
        ostream & toStream(ostream & flux) const;

        /** 
         * @brief UnSerialize the ReflectionListFactory.
         * 
         * @param flux The stream to load the ReflectionListFactory from.
         * 
         * @return The flux without the ReflectionListFactory un-serialized.
         */
        istream & fromStream(istream & flux);

      protected:

        Geometry & _geometry;

        ReflectionFactory * _reflectionFactory;

        vector<Reflection *> _reflections;

        /** 
         * @brief Default constructor
         * 
         * @param geometry The Geometry related to the Reflection 
         */
        ReflectionListFactory(Geometry & geometry);

        ReflectionListFactory(ReflectionListFactory const & factory);
      };

} // namespace hkl

static ostream &
operator<<(ostream & flux, hkl::ReflectionListFactory const & factory)
{
    return factory.printToStream(flux);
}

#endif // _MONOCRYSTALREFLECTIONLISTFACTORY_H_
