#ifndef _CRYSTALLIST_H_
#define _CRYSTALLIST_H_

#include "config.h"

#include <string>
#include <iostream>

#include "crystal.h"
#include "mymap.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

  /**
   * @brief A class design to describe a CrystalList with allways a default
   * crystal in it.
   */
  class CrystalList : public MyMap<Crystal>
  {
    public:
  
      CrystalList(void); //!< The default constructor
  
      /**
       * @brief The Copy constructor
       * @param crystalList a CrystalList to copy from.
       */
      CrystalList(CrystalList const & crystalList);
  
      virtual ~CrystalList(void); //!< The default destructor
      
      /**
       * \brief remove a crystal from the crystal list.
       * \param name The name of the crystall to remove.
       *
       * if the removed crystal was the last on, an empty crystal is
       * automatically add to the list.
       */
      void remove(string const & name) throw (HKLException);
    
      /**
       * \brief Clear the crystal list.
       *
       * Clear all crystals an add the default one.
       */
      void clear(void);
  };

} // namespace hkl

/**
 * @brief Overload of the << operator for the CrystalList class
 * @param flux
 * @param crystalList.
 * @return the modified flux.
 */
//std::ostream & operator<<(std::ostream & flux, hkl::CrystalList const & crystalList); 

#endif // _CRYSTALLIST_H_
