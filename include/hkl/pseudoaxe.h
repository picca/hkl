#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include "config.h"

#include <iostream>

#include "mymap.h"
#include "geometry.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {

  /*!
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
      virtual void initialize(Geometry const & geometry) throw (HKLException) = 0;
      
      /**
       * \brief Uninitialize the PseudoAxe from the Geometry.
       */
      virtual void uninitialize(void) {m_wasInitialized = false;}
      
      /** 
       * @brief Is a PseudoAxe valid ?
       * 
       * @return The validity of the PseudoAxe for the current Geometry.
       *
       * A pseudoAxe is valid when its value can be compute and when the meaning
       * of this value is coherant with the initialization of the pseudoAxe.
       */
      virtual bool get_isValid(Geometry const & geometry) const = 0;

      /**
       * \brief get the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \return the position of the PseudoAxe.
       */
      virtual double get_value(Geometry const & geometry) const throw (HKLException) = 0;

      /**
       * \brief set the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \param value The value to set.
       * \throw HKLException if the pseudoAxe is not ready to be set.
       */
      virtual void set_value(Geometry & geometry, double const & value) const throw (HKLException) = 0;
    
    protected:
  
      bool m_wasInitialized; //!< Tell if the PseudoAxe was initialized before using it.

      PseudoAxe(void); //!< The default constructor - protected to make sure this class is abstract.
  };

#ifdef MSVC6
  typedef MyStarMap<PseudoAxe*> PseudoAxeList;
#else
  typedef MyMap<PseudoAxe*> PseudoAxeList; //!< \typedef A MyMap containing pointers of Pseudoaxe.
#endif

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
ostream & operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe); 


#endif // _PSEUDOAXE_H_
