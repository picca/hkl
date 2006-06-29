#ifndef _CRYSTALLIST_H_
#define _CRYSTALLIST_H_

#include <iostream>

#include "mymap.h"
#include "crystal.h"
#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

    /**
     * @brief A class design to describe a CrystalList with allways a default
     * crystal in it.
     */
    template<typename T>
    class CrystalList : public MyMap<Crystal<T> >
      {
      public:
        CrystalList(void);
        CrystalList(CrystalList const & crystalList);
        virtual ~CrystalList(void); //!< The default destructor
        void remove(MyString const & name) throw (HKLException);
        void clear(void);
      };

    /** 
     * @brief The default constructor
     * 
     * @return 
     */
    template<typename T>
    CrystalList<T>::CrystalList(void)
    : MyMap<Crystal<T> >()
      {
        MyMap<Crystal<T> >::add(Crystal<T>(DEFAULT_CRYSTAL_NAME));
      }

    /**
     * @brief The Copy constructor
     * @param crystalList a CrystalList to copy from.
     */
    template<typename T>
    CrystalList<T>::CrystalList(CrystalList const & crystalList) :
      MyMap<Crystal<T> >(crystalList)
    {}

    template<typename T>
    CrystalList<T>::~CrystalList(void)
      {}

    /**
     * \brief remove a crystal from the crystal list.
     * \param name The name of the crystall to remove.
     *
     * if the removed crystal was the last on, an empty crystal is
     * automatically add to the list.
     */
    template<typename T>
    void
    CrystalList<T>::remove(MyString const & name) throw (HKLException)
      {
        MyMap<Crystal<T> >::remove(name);
        if (MyMap<Crystal<T> >::size() == 0)
            MyMap<Crystal<T> >::add(Crystal<T> (DEFAULT_CRYSTAL_NAME));
      }

    /**
     * \brief Clear the crystal list.
     *
     * Clear all crystals an add the default one.
     */
    template<typename T>
    void
    CrystalList<T>::clear(void)
      {
        MyMap<Crystal<T> >::clear();
        MyMap<Crystal<T> >::add(Crystal<T> (DEFAULT_CRYSTAL_NAME));
      }


} // namespace hkl

#endif // _CRYSTALLIST_H_
