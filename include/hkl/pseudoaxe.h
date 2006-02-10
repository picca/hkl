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
       * @brief Get the m_wasInitialized parameter
       * 
       * @return The value of m_wasInitialized.
       *
       * A pseudoAxe is initialized after the user call the init method.
       */
      bool const & get_wasInitialized(void) const {return m_wasInitialized;}

      /** 
       * @brief Get the m_isValid parameter.
       * 
       * @return The value of m_isValid.
       *
       * A pseudoAxe is valid when its value can be compute and when the meaning
       * of this value is coherant with the initialization of the pseudoAxe.
       */
      bool const & get_isValid(void) const {return m_isValid;}

      /** 
       * @brief Set the m_wasInitialized flag
       * 
       * @param flag The falg to set.
       */
      void set_wasInitialized(bool const & flag) {m_wasInitialized = flag;}
      
      /** 
       * @brief Set the m_isValid flag
       * 
       * @param flag The falg to set
       */
      void set_isValid(bool const & flag) {m_isValid = flag;}
      
      /*!
       * \brief Initialize the PseudoAxe from the Geometry.
       * \param geometry The configuration to save for calculation.
       */
      virtual void init(Geometry const & geometry) = 0;
      
      /*!
       * \brief get the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \return the position of the PseudoAxe.
       */
      virtual double const get_value(Geometry const & geometry) = 0;

      /*!
       * \brief set the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \param value The value to set.
       * \throw HKLException if the pseudoAxe is not ready to be set.
       */
      virtual void set_value(Geometry & geometry, double value) throw (HKLException) = 0;
    
    protected:
  
      bool m_wasInitialized; //!< Tell if the PseudoAxe was initialized before using it.
      bool m_isValid; //!< Tell if the pseudoAxe is valid or not. 

      PseudoAxe(void); //!< The default constructor - protected to make sure this class is abstract.
  };

#ifdef VCPP6
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
