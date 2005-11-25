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
       * \brief Are two PseudoAxe equals ?
       * \param pseudoAxe The PseudoAxe to compare with.
       * \return false if both PseudoAxe are different.
       */
      bool operator==(PseudoAxe const & pseudoAxe) const;

      /**
       * \brief print the PseudoAxe into a flux
       * \param flux The stream to print into.
       * \return The flux containing the PseudoAxe in a human reading string.
       */
      std::ostream & printToStream(std::ostream & flux) const;

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
  typedef MyStarVector<PseudoAxe*> PseudoAxeList;
#else
  typedef MyVector<PseudoAxe*> PseudoAxeList;
#endif

} // namespace hkl

/**
 * \brief Overload of the << operator for the PseudoAxe class
 */
ostream & operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe); 


#endif // _PSEUDOAXE_H_
