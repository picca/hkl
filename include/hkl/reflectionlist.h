#ifndef _REFLECTIONLIST_H_
#define _REFLECTIONLIST_H_

#include "reflectionfactory.h"

using namespace std;

namespace hkl
  {

  class ReflectionList
    {
    public:

      /**
       * @brief Default constructor
       * @param geometry The Geometry related to the Reflection 
       * @param type The type of the Reflection in the ReflectionList.
       */
      ReflectionList(Geometry & geometry, ReflectionType const & type);

      /**
       * @brief The copy constructor.
       * @param factory The factory to copy from.
       */
      ReflectionList(ReflectionList const & factory);

      /**
       * @brief The default destructor.
       */
      virtual ~ReflectionList(void);

      /**
       * @brief Make a deep copy of a ReflectionList.
       * 
       * @return A pointer on the copied ReflectionList.
       */
      virtual ReflectionList * clone(void);

      /**
       * @brief Add a reflection to the ReflectionList.
       * @param hkl The scattering vector of the added reflection.
       * @return A reference on the added reflection.
       */
      Reflection & add(svector const & hkl) throw (HKLException);

      /**
       * @brief Delete the ith reflection
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
      Reflection * operator[](unsigned int index) throw (HKLException);

      /**
       * @brief Get an iterator on the first element of ReflectionList.
       * @return The iterator.
       */
      vector<Reflection *>::iterator begin(void)
      {
        return _reflections.begin();
      }

      /**
       * @brief Get an iterator on the end of ReflectionList.
       * @return The iterator.
       */
      vector<Reflection *>::iterator end(void)
      {
        return _reflections.end();
      }

      /**
       * @brief Are two ReflectionList equals ?
       * @param reflectionListFactory the ReflectionList to compare with.
       * @return True if both are equals, false otherwise.
       */
      bool operator==(ReflectionList const & reflectionListFactory) const;

      /**
       * @brief print the ReflectionList into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Serialize the ReflectionList.
       * @param  flux The stream to save the ReflectionList into.
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

      Geometry & _geometry; //!< The Geometry use to populate the Reflection.

      ReflectionFactory * _reflectionFactory; //!< The factory use to create the Reflections.

      vector<Reflection *> _reflections; //!< This vector contain all the Reflections.

    };

} // namespace hkl

static ostream &
operator<<(ostream & flux, hkl::ReflectionList const & factory)
{
  return factory.printToStream(flux);
}

#endif // _REFLECTIONLIST_H_
