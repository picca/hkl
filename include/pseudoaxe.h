#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include "config.h"

#include <iostream>

#include "myvector.h"
#include "geometry.h"
#include "objectwithparameters.h"
#include "mode.h"

using namespace std;

namespace hkl {

  /**
   * \brief A class design to describe a pseudoaxe.
   */
  class PseudoAxe : public ObjectWithParameters
  {      
    public:

      virtual ~PseudoAxe(void); //!< The default destructor.

      /**
       * \brief Initialize the PseudoAxe from the Geometry.
       * \param geometry The configuration to save for calculation.
       */
      virtual void init(Geometry const & geometry) = 0;
          
      /**
       * \brief get the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \return the position of the PseudoAxe.
       */
      virtual double const get_value(Geometry const & geometry) const = 0;

      /**
       * \brief set the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \param value The value to set.
       */
      virtual void set_value(Geometry & geometry, double value) throw (HKLException) = 0;
    
    protected:
    
      PseudoAxe(void); //!< The default constructor - protected to make sure this class is abstract.
  };

#ifdef VCPP6
  typedef MyStarMap<PseudoAxe*> PseudoAxeList;
#else
  typedef MyMap<PseudoAxe*> PseudoAxeList;
#endif

} // namespace hkl

/**
 * \brief Overload of the << operator for the PseudoAxe class
 */
ostream & operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe); 


#endif // _PSEUDOAXE_H_
