#ifndef _REFLECTIONLIST_H_
#define _REFLECTIONLIST_H_

#include "enums.h"
#include "reflectionfactory.h"

using namespace std;

namespace hkl {

    class ReflectionList
      {
      public:

        /** 
         * @brief Default constructor
         * 
         * @param geometry The Geometry related to the Reflection 
         */
        ReflectionList(Geometry & geometry, ReflectionType const & type);

        ReflectionList(ReflectionList const & factory);

        virtual ~ReflectionList(void);

        /** 
         * @brief Make a deep copy of a ReflectionList.
         * 
         * @return A pointer on the copied ReflectionList.
         */
        virtual ReflectionList * clone(void);

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

        bool operator==(ReflectionList const & reflectionListFactory) const;

        ostream & printToStream(ostream & flux) const;

        /** 
         * @brief Serialize the ReflectionList.
         * 
         * @param  flux The stream to save the ReflectionList into.
         * 
         * @return the flux with the ReflectionList serialized. 
         */
        ostream & toStream(ostream & flux) const;

        /** 
         * @brief UnSerialize the ReflectionList.
         * 
         * @param flux The stream to load the ReflectionList from.
         * 
         * @return The flux without the ReflectionList un-serialized.
         */
        istream & fromStream(istream & flux);

      protected:

        Geometry & _geometry;

        ReflectionFactory * _reflectionFactory;

        vector<Reflection *> _reflections;

      };

} // namespace hkl

static ostream &
operator<<(ostream & flux, hkl::ReflectionList const & factory)
{
    return factory.printToStream(flux);
}

#endif // _REFLECTIONLIST_H_
